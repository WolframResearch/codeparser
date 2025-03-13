#![allow(non_snake_case)]

#[cfg(all(not(feature = "USE_MATHLINK"), not(feature = "USE_EXPR_LIB")))]
compile_error!("codeparser-wll requires that either USE_MATHLINK or USE_EXPR_LIB are set");

#[cfg(feature = "USE_MATHLINK")]
mod convert_wstp;

#[cfg(feature = "USE_EXPR_LIB")]
mod convert_expr;

#[cfg(feature = "USE_MATHLINK")]
mod from_expr;


use wolfram_library_link::{
    self as wll,
    expr::{self, Expr},
    sys::mint,
    wstp,
};

use wolfram_parser::{
    abstract_::{abstract_, Aggregate},
    node::Node,
    parser_session::ParserSession,
    quirks::QuirkSettings,
    source::{SourceConvention, DEFAULT_TAB_WIDTH},
    symbol_registration::{SYMBOL_LIST, SYMBOL_NULL},
    Container, ContainerBody, EncodingMode, FirstLineBehavior, StringifyMode,
};

use crate::{convert_wstp::WstpPut, from_expr::FromExpr};


pub(crate) type ParserSessionPtr = *mut std::ffi::c_void;

#[cfg(feature = "USE_EXPR_LIB")]
type expr = *mut std::ffi::c_void;

pub(crate) const SIZEOF_VOID_P: usize = std::mem::size_of::<*mut std::ffi::c_void>();

//======================================
// Types
//======================================

//======================================
// API Functions
//======================================

// TODO: Remove this, because the parser session is not reused?
// Though there might be some state?
fn CreateParserSession() -> ParserSessionPtr {
    // return Box::into_raw(Box::new(ParserSession::new()));
    std::ptr::null_mut()
}

unsafe fn DestroyParserSession(session: ParserSessionPtr) {
    drop(Box::from_raw(session))
}

// fn ParserSessionInit(
//     session: ParserSessionPtr,
//     buf: Buffer,
//     bufLen: usize,
//     libData: WolframLibraryData,
//     srcConvention: SourceConvention,
//     tabWidth: u32,
//     firstLineBehavior: FirstLineBehavior,
//     encodingMode: EncodingMode,
// ) -> c_int {
//     let bufAndLen: BufferAndLength = BufferAndLength { buf, len: bufLen };

//     return session.init(bufAndLen, libData, srcConvention, tabWidth, firstLineBehavior, encodingMode);
// }

// fn ParserSessionParseExpressions(session: ParserSessionPtr) -> NodeContainerPtr {
//     return session.concrete_parse_expressions();
// }

// fn ParserSessionTokenize(session: ParserSessionPtr) -> NodeContainerPtr {
//     return session.tokenize();
// }

// fn ParserSessionConcreteParseLeaf(session: ParserSessionPtr, mode: StringifyMode) -> NodeContainerPtr {
//     return session.concreteParseLeaf(mode);
// }

// fn ParserSessionSafeString(session: ParserSessionPtr) -> NodeContainerPtr {
//     return session.safeString();
// }

// fn ParserSessionReleaseNodeContainer(session: ParserSessionPtr, C: NodeContainerPtr) {
//     session.releaseNodeContainer(C);
// }

// TODO: Display
// fn NodeContainerPrint(C: NodeContainerPtr, s: &mut std::ostream) {
//     C.print(s);
// }

// fn NodeContainerCheck(C: NodeContainerPtr) -> c_int {
//     return C.check();
// }

#[cfg(feature = "USE_EXPR_LIB")]
fn NodeContainerToExpr(session: ParserSessionPtr, C: NodeContainerPtr) -> expr {
    return C.toExpr(session);
}

#[no_mangle]
extern "C" fn WolframLibrary_getVersion() -> mint {
    return wll::sys::WolframLibraryVersion as mint;
}

// #if USE_EXPR_LIB
#[cfg(feature = "USE_EXPR_LIB")]
#[wll::export]
fn CreateParserSession_LibraryLink() -> mint {
    let session: *mut _ = CreateParserSession();

    let e: *mut _ = Expr_FromInteger64(session as mint);

    return e as mint;
}

// #elif USE_MATHLINK
// #[cfg(feature = "USE_MATHLINK")]
#[wll::export(wstp)]
pub fn CreateParserSession_LibraryLink(link: &mut wstp::Link) {
    if link.get_type().unwrap() != wstp::TokenType::Function {
        panic!("expected List expr");
    }

    let len: usize = match link.test_head("List") {
        Ok(len) => len,
        Err(err) => panic!("expected List expr: {err}"),
    };

    if len != 0 {
        todo!()
    }

    let session = CreateParserSession();

    if SIZEOF_VOID_P == 8 {
        link.put_i64(session as i64)
            .expect("unable to write session to link");
    } else if SIZEOF_VOID_P == 4 {
        link.put_i32(session as i32)
            .expect("unable to write session to link");
    } else {
        panic!("unknown pointer size")
    }
}

#[cfg(feature = "USE_EXPR_LIB")]
#[wll::export]
fn DestroyParserSession_LibraryLink(mlSession: mint) -> mint {
    let session = mlSession as ParserSessionPtr;

    DestroyParserSession(session);

    //
    // cannot use session after this
    //

    let e = Expr_MEncodedStringToSymbolExpr(SYMBOL_NULL.as_str());

    return e as mint;
}

/// FIXME: Currently all the results read from this function are unused, because
///        ParserSession's are not ephemeral things whose lifetimes tie theme
///        to the scope of the current parser operation. Remove this function
///        and the unused `session` arguments passed over WSTP?
unsafe fn read_session_from_link(link: &mut wstp::Link) -> ParserSessionPtr {
    let session: usize = if SIZEOF_VOID_P == 8 {
        let mlSession: i64 = link.get_i64().unwrap();
        usize::try_from(mlSession).expect("unable to convert i64 to usize")
    } else if SIZEOF_VOID_P == 4 {
        let mlSession: i32 = link.get_i32().unwrap();
        usize::try_from(mlSession).expect("unable to convert i32 to usize")
    } else {
        panic!("unknown pointer size")
    };

    let session = session as ParserSessionPtr;

    session
}

fn read_input_bytes(link: &mut wstp::Link) -> Vec<u8> {
    let len = match link.test_head("ByteArray") {
        Ok(len) => len,
        Err(err) => panic!("expected ByteArray: {err}"),
    };

    if len != 1 {
        panic!("input byte array has incorrect format: expected 1 argument, got {len}")
    }

    let arr: wstp::Array<u8> = link.get_u8_array().expect("unable to read Array of u8");

    assert_eq!(arr.rank(), 1);

    // Avoid holding a `&mut link`
    // TODO: This clone would not be necessary if the input data was passed as
    //       a native &NumericArray<u8>. (Though doing so would require
    //       returning the output as WXF)
    let buffer: Vec<u8> = arr.data().to_vec();

    buffer
}

// #[cfg(feature = "USE_MATHLINK")]
#[wll::export(wstp)]
pub fn DestroyParserSession_LibraryLink(link: &mut wstp::Link) {
    let len = match link.test_head("List") {
        Ok(len) => len,
        Err(err) => panic!("expected List: {err}"),
    };

    if len != 1 {
        todo!()
    }

    if link.get_type().unwrap() != wstp::TokenType::Function {
        todo!()
    }

    let session = unsafe { read_session_from_link(link) };

    unsafe { DestroyParserSession(session) };

    //
    // cannot use session after this
    //

    link.put_symbol(SYMBOL_NULL.as_str()).unwrap();
}

//==========================================================
// Abstract Parsing
//==========================================================

//======================================
// Aggregate_LibraryLink
//======================================

#[cfg(feature = "USE_MATHLINK")]
#[wll::export(wstp)]
pub fn Aggregate_LibraryLink(link: &mut wstp::Link) {
    let mut args: Vec<Expr> = parse_assuming_link_print_full_symbols(link);

    if args.len() != 1 {
        panic!("unexpected number of arguments passed to Aggregate: {args:?}")
    }

    let arg = args.remove(0);

    if arg.has_normal_head(&expr::Symbol::new("System`Failure")) {
        link.put_expr(&arg).unwrap();
        return;
    }

    let Container {
        kind,
        body,
        metadata,
    } = match Container::from_expr(&arg) {
        Ok(container) => container,
        Err(err) => panic!("Error parsing '{arg}': {err}"),
    };

    let body = match body {
        ContainerBody::Nodes(nodes) => ContainerBody::Nodes(Aggregate(nodes)),
        ContainerBody::Missing(_) => body,
    };

    let container = Container {
        kind,
        body,
        metadata,
    };

    debug_assert!(!link.is_ready());

    container.put(link);
}

//======================================
// Abstract_LibraryLink
//======================================

#[cfg(feature = "USE_MATHLINK")]
#[wll::export(wstp)]
pub fn Abstract_LibraryLink(link: &mut wstp::Link) {
    let mut args: Vec<Expr> = parse_assuming_link_print_full_symbols(link);

    if args.len() != 1 {
        panic!("unexpected number of arguments passed to Abstract: {args:?}")
    }

    let arg = args.remove(0);

    let node = match Node::from_expr(&arg) {
        Ok(node) => abstract_(node),
        Err(err) => panic!("Error parsing arg: {err}: (expr: {arg})"),
    };

    node.put(link);

    // let Container {
    //     kind,
    //     body,
    //     metadata,
    // } = match Container::from_expr(&arg) {
    //     Ok(container) => container,
    //     Err(err) => panic!("Error parsing '{arg}': {err}"),
    // };

    // let body = match body {
    //     ContainerBody::Nodes(nodes) => ContainerBody::Nodes(Abstract(nodes)),
    //     ContainerBody::Missing(_) => body,
    // };

    // let container = Container {
    //     kind,
    //     body,
    //     metadata,
    // };

    // debug_assert!(!link.is_ready());

    // container.put(link);
}

//==========================================================
// Concrete Parsing
//==========================================================

//======================================
// ConcreteParseBytes
//======================================

#[cfg(feature = "USE_EXPR_LIB")]
#[no_mangle]
pub fn ConcreteParseBytes_LibraryLink(
    libData: WolframLibraryData,
    Argc: mint,
    Args: *mut MArgument,
    Res: MArgument,
) -> c_int {
    if (Argc != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }

    let mlSession = MArgument_getInteger(Args[0]);
    let session = mlSession as ParserSessionPtr;

    let arr = ScopedNumericArray(libData, Args[1]);

    let mlSrcConvention = MArgument_getInteger(Args[2]);
    let srcConvention =
        SourceConvention::try_from(mlSrcConvention).expect("invalid SourceConvention value");

    let mlTabWidth = MArgument_getInteger(Args[3]);
    let tabWidth = mlTabWidth as u32;

    let mlFirstLineBehavior = MArgument_getInteger(Args[4]);
    let firstLineBehavior =
        FirstLineBehavior::try_from(mlFirstLineBehavior).expect("invalid FirstLineBehavior value");

    let numBytes = arr.len();

    let data = arr.data();

    if (ParserSessionInit(
        session,
        data,
        numBytes,
        libData,
        srcConvention,
        tabWidth,
        firstLineBehavior,
        EncodingMode::Normal,
    )) {
        return LIBRARY_FUNCTION_ERROR;
    }

    let C = ParserSessionParseExpressions(session);

    let e = NodeContainerToExpr(session, C);

    ParserSessionReleaseNodeContainer(session, C);

    ParserSessionDeinit(session);

    MArgument_setInteger(Res, e as mint);

    return LIBRARY_NO_ERROR;
}

// #elif USE_MATHLINK
#[cfg(feature = "USE_MATHLINK")]
#[wll::export(wstp)]
pub fn ConcreteParseBytes_LibraryLink(link: &mut wstp::Link) {
    let len = match link.test_head("List") {
        Ok(len) => len,
        Err(err) => panic!("expected List: {err}"),
    };

    if len != 6 {
        panic!("wrong arg count: {len}")
    }

    let _session = unsafe { read_session_from_link(link) };

    let buffer = read_input_bytes(link);

    let mlSrcConvention = link.get_i32().unwrap();
    let srcConvention =
        SourceConvention::try_from(mlSrcConvention).expect("invalid SourceConvention value");

    let tabWidth =
        u32::try_from(link.get_i64().unwrap()).expect("unable to convert tab width value to u32");

    let mlFirstLineBehavior = link.get_i32().unwrap();
    let firstLineBehavior =
        FirstLineBehavior::try_from(mlFirstLineBehavior).expect("invalid FirstLineBehavior value");

    let quirk_settings =
        QuirkSettings::from_expr(&get_expr(link).unwrap()).expect("invalid quirks settings value");

    link.new_packet().unwrap();

    let mut session = ParserSession::new(
        &buffer,
        srcConvention,
        tabWidth,
        firstLineBehavior,
        EncodingMode::Normal,
        quirk_settings,
    );

    session.concrete_parse_expressions().put(link);

    drop(session);
}

//======================================
// ConcreteParseFile
//======================================

/*
#[cfg(feature = "USE_EXPR_LIB")]
DLLEXPORT int ConcreteParseFile_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

    if (Argc != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }

    auto mlSession = MArgument_getInteger(Args[0]);
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);

    auto full = ScopedUTF8String(libData, Args[1]);

    auto mlSrcConvention = MArgument_getInteger(Args[2]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);

    auto mlTabWidth = MArgument_getInteger(Args[3]);
    auto tabWidth = static_cast<uint32_t>(mlTabWidth);

    auto mlFirstLineBehavior = MArgument_getInteger(Args[4]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);

    if !validatePath(&path) {
        panic!("insufficient permissions to read file: {path}");
    }

    auto fb = ScopedFileBuffer(full.data(), strlen(reinterpret_cast<const char *>(full.data())));

    auto numBytes = fb.getLen();

    auto data = fb.getBuf();

    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = true;

    if (ParserSessionInit(session, data, numBytes, libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }

    auto C = ParserSessionParseExpressions(session);

    auto e = NodeContainerToExpr(session, C);

    ParserSessionReleaseNodeContainer(session, C);

    ParserSessionDeinit(session);

    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
*/

#[cfg(feature = "USE_MATHLINK")]
#[wll::export(wstp)]
fn ConcreteParseFile_LibraryLink(link: &mut wstp::Link) {
    let len = match link.test_head("List") {
        Ok(len) => len,
        Err(err) => panic!("expected List: {err}"),
    };

    if len != 6 {
        panic!()
    }

    let _session = unsafe { read_session_from_link(link) };

    let path: String = link.get_string().unwrap();

    if !validatePath(&path) {
        panic!("insufficient permissions to read file: {path}");
    }

    let mlSrcConvention = link.get_i32().unwrap();
    let srcConvention =
        SourceConvention::try_from(mlSrcConvention).expect("invalid SourceConvention value");

    let tabWidth =
        u32::try_from(link.get_i64().unwrap()).expect("unable to convert tab width value to u32");

    let mlFirstLineBehavior = link.get_i32().unwrap();
    let firstLineBehavior =
        FirstLineBehavior::try_from(mlFirstLineBehavior).expect("invalid FirstLineBehavior value");

    let quirk_settings =
        QuirkSettings::from_expr(&get_expr(link).unwrap()).expect("invalid quirks settings value");

    link.new_packet().unwrap();

    let bytes = match std::fs::read(path) {
        Ok(bytes) => bytes,
        Err(err) => todo!("FIXME: {err:?}"),
    };

    let mut session = ParserSession::new(
        bytes.as_slice(),
        srcConvention,
        tabWidth,
        firstLineBehavior,
        EncodingMode::Normal,
        quirk_settings,
    );

    session.concrete_parse_expressions().put(link);

    drop(session);
}

//==========================================================
// Tokenize Parsing
//==========================================================

//======================================
// TokenizeBytes
//======================================

#[cfg(feature = "USE_EXPR_LIB")]
fn TokenizeBytes_LibraryLink(
    libData: WolframLibraryData,
    Argc: mint,
    Args: *mut MArgument,
    Res: MArgument,
) -> c_int {
    if (Argc != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }

    let mlSession = MArgument_getInteger(Args[0]);
    let session = mlSession as ParserSessionPtr;

    let arr = ScopedNumericArray(libData, Args[1]);

    let mlSrcConvention = MArgument_getInteger(Args[2]);
    let srcConvention =
        SourceConvention::try_from(mlSrcConvention).expect("invalid SourceConvention value");

    let mlTabWidth = MArgument_getInteger(Args[3]);
    let tabWidth = mlTabWidth as c_int;

    let mlFirstLineBehavior = MArgument_getInteger(Args[4]);
    let firstLineBehavior =
        FirstLineBehavior::try_from(mlFirstLineBehavior).expect("invalid FirstLineBehavior value");

    let numBytes = arr.len();

    let data = arr.data();

    if (ParserSessionInit(
        session,
        data,
        numBytes,
        libData,
        srcConvention,
        tabWidth,
        firstLineBehavior,
        EncodingMode::Normal,
    )) {
        return LIBRARY_FUNCTION_ERROR;
    }

    let C = ParserSessionTokenize(session);

    let e = NodeContainerToExpr(session, C);

    ParserSessionReleaseNodeContainer(session, C);

    ParserSessionDeinit(session);

    MArgument_setInteger(Res, e as mint);

    return LIBRARY_NO_ERROR;
}

#[cfg(feature = "USE_MATHLINK")]
#[wll::export(wstp)]
fn TokenizeBytes_LibraryLink(link: &mut wstp::Link) {
    let len = match link.test_head("List") {
        Ok(len) => len,
        Err(err) => panic!("expected List: {err};"),
    };

    if len != 6 {
        panic!("unexpected number of arguments: {len}");
    }

    let _session = unsafe { read_session_from_link(link) };

    let buffer: Vec<u8> = read_input_bytes(link);

    let mlSrcConvention = link.get_i32().unwrap();
    let srcConvention =
        SourceConvention::try_from(mlSrcConvention).expect("invalid SourceConvention value");

    let tabWidth =
        u32::try_from(link.get_i64().unwrap()).expect("unable to convert tab width value to u32");

    let mlFirstLineBehavior = link.get_i32().unwrap();
    let firstLineBehavior =
        FirstLineBehavior::try_from(mlFirstLineBehavior).expect("invalid FirstLineBehavior value");

    let quirk_settings =
        QuirkSettings::from_expr(&get_expr(link).unwrap()).expect("invalid quirks settings value");

    link.new_packet().unwrap();

    let mut session = ParserSession::new(
        &buffer,
        srcConvention,
        tabWidth,
        firstLineBehavior,
        EncodingMode::Normal,
        quirk_settings,
    );

    match session.tokenize() {
        Ok(nodes) => nodes.put(link),
        Err(flag) => {
            link.put_function(SYMBOL_LIST.as_str(), 1).unwrap();
            flag.put(link);
        },
    };

    drop(session);
}

//======================================
// TokenizeFile
//======================================

/*
#[cfg(feature = "USE_EXPR_LIB")]
#[wll::export]
fn TokenizeFile_LibraryLink(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {

    if (Argc != 5) {
        return LIBRARY_FUNCTION_ERROR;
    }

    auto mlSession = MArgument_getInteger(Args[0]);
    auto session = reinterpret_cast<ParserSessionPtr>(mlSession);

    auto full = ScopedUTF8String(libData, Args[1]);

    auto mlSrcConvention = MArgument_getInteger(Args[2]);
    auto srcConvention = static_cast<SourceConvention>(mlSrcConvention);

    auto mlTabWidth = MArgument_getInteger(Args[3]);
    auto tabWidth = static_cast<int>(mlTabWidth);

    auto mlFirstLineBehavior = MArgument_getInteger(Args[4]);
    auto firstLineBehavior = static_cast<FirstLineBehavior>(mlFirstLineBehavior);

    if !validatePath(&path) {
        panic!("insufficient permissions to read file: {path}");
    }

    auto fb = ScopedFileBuffer(full.data(), strlen(reinterpret_cast<const char *>(full.data())));

    auto numBytes = fb.getLen();

    auto data = fb.getBuf();

    ParserSessionOptions opts;
    opts.srcConvention = srcConvention;
    opts.tabWidth = tabWidth;
    opts.firstLineBehavior = firstLineBehavior;
    opts.encodingMode = ENCODINGMODE_NORMAL;
    opts.alreadyHasEOFSentinel = true;

    if (ParserSessionInit(session, data, numBytes, libData, opts)) {
        return LIBRARY_FUNCTION_ERROR;
    }

    auto C = ParserSessionTokenize(session);

    auto e = NodeContainerToExpr(session, C);

    ParserSessionReleaseNodeContainer(session, C);

    ParserSessionDeinit(session);

    MArgument_setInteger(Res, reinterpret_cast<mint>(e));

    return LIBRARY_NO_ERROR;
}
*/

#[cfg(feature = "USE_MATHLINK")]
#[wll::export(wstp)]
fn TokenizeFile_LibraryLink(link: &mut wstp::Link) {
    let len = match link.test_head("List") {
        Ok(len) => len,
        Err(err) => panic!("expected List: {err}"),
    };

    if len != 6 {
        panic!()
    }

    let _session = unsafe { read_session_from_link(link) };

    let path: String = link.get_string().unwrap();

    if !validatePath(&path) {
        panic!("insufficient permissions to read file: {path}");
    }

    let mlSrcConvention = link.get_i32().unwrap();
    let srcConvention =
        SourceConvention::try_from(mlSrcConvention).expect("invalid SourceConvention value");

    let tabWidth =
        u32::try_from(link.get_i64().unwrap()).expect("unable to convert tab width value to u32");

    let mlFirstLineBehavior = link.get_i32().unwrap();
    let firstLineBehavior =
        FirstLineBehavior::try_from(mlFirstLineBehavior).expect("invalid FirstLineBehavior value");

    let quirk_settings =
        QuirkSettings::from_expr(&get_expr(link).unwrap()).expect("invalid quirks settings value");

    link.new_packet().unwrap();

    let bytes = match std::fs::read(path) {
        Ok(bytes) => bytes,
        Err(err) => todo!("FIXME: {err:?}"),
    };

    let mut session = ParserSession::new(
        bytes.as_slice(),
        srcConvention,
        tabWidth,
        firstLineBehavior,
        EncodingMode::Normal,
        quirk_settings,
    );

    match session.tokenize() {
        Ok(nodes) => nodes.put(link),
        Err(flag) => {
            link.put_function(SYMBOL_LIST.as_str(), 1).unwrap();
            flag.put(link);
        },
    };

    drop(session);
}

//==========================================================
// Concrete Leaf Parsing
//==========================================================

//======================================
// ConcreteParseLeaf
//======================================

#[cfg(feature = "USE_EXPR_LIB")]
fn ConcreteParseLeaf_LibraryLink(
    libData: WolframLibraryData,
    Argc: mint,
    Args: *mut MArgument,
    Res: MArgument,
) -> c_int {
    if (Argc != 7) {
        return LIBRARY_FUNCTION_ERROR;
    }

    let mlSession = MArgument_getInteger(Args[0]);
    let session = mlSession as ParserSessionPtr;

    let arr = ScopedNumericArray(libData, Args[1]);

    let stringifyMode = MArgument_getInteger(Args[2]);

    let mlSrcConvention = MArgument_getInteger(Args[3]);
    let srcConvention =
        SourceConvention::try_from(mlSrcConvention).expect("invalid SourceConvention value");

    let mlTabWidth = MArgument_getInteger(Args[4]);
    let tabWidth = mlTabWidth as _cint;

    let mlFirstLineBehavior = MArgument_getInteger(Args[5]);
    let firstLineBehavior =
        FirstLineBehavior::try_from(mlFirstLineBehavior).expect("invalid FirstLineBehavior value");

    let mlEncodingMode = MArgument_getInteger(Args[6]);
    let encodingMode = EncodingMode::try_from(mlEncodingMode).expect("invalid EncodingMode value");

    let numBytes = arr.len();

    let data = arr.data();

    if (ParserSessionInit(
        session,
        data,
        numBytes,
        libData,
        srcConvention,
        tabWidth,
        firstLineBehavior,
        encodingMode,
    )) {
        return LIBRARY_FUNCTION_ERROR;
    }

    let C = ParserSessionConcreteParseLeaf(
        session,
        StringifyMode::try_from(stringifyMode).expect("invalid StringifyMode value"),
    );

    let e = NodeContainerToExpr(session, C);

    ParserSessionReleaseNodeContainer(session, C);

    ParserSessionDeinit(session);

    MArgument_setInteger(Res, e as mint);

    return LIBRARY_NO_ERROR;
}

#[cfg(feature = "USE_MATHLINK")]
#[wll::export(wstp)]
fn ConcreteParseLeaf_LibraryLink(link: &mut wstp::Link) {
    let len = match link.test_head("List") {
        Ok(len) => len,
        Err(err) => panic!("expected List: {err}"),
    };

    if len != 8 {
        panic!()
    }

    let _session = unsafe { read_session_from_link(link) };

    let buffer: Vec<u8> = read_input_bytes(link);

    let stringifyMode = link.get_i32().unwrap();

    let mlSrcConvention = link.get_i32().unwrap();
    let srcConvention =
        SourceConvention::try_from(mlSrcConvention).expect("invalid SourceConvention value");

    let tabWidth =
        u32::try_from(link.get_i64().unwrap()).expect("unable to convert tab width value to u32");

    let mlFirstLineBehavior = link.get_i32().unwrap();
    let firstLineBehavior =
        FirstLineBehavior::try_from(mlFirstLineBehavior).expect("invalid FirstLineBehavior value");

    let mlEncodingMode = link.get_i32().unwrap();
    let encodingMode = EncodingMode::try_from(mlEncodingMode).expect("invalid EncodingMode value");

    let quirk_settings =
        QuirkSettings::from_expr(&get_expr(link).unwrap()).expect("invalid quirks settings value");

    link.new_packet().unwrap();

    let mut session = ParserSession::new(
        &buffer,
        srcConvention,
        tabWidth,
        firstLineBehavior,
        encodingMode,
        quirk_settings,
    );

    let result = session.concreteParseLeaf(
        StringifyMode::try_from(stringifyMode).expect("invalid StringifyMode value"),
    );

    result.put(link);

    drop(session);
}

//======================================
// SafeString
//======================================

#[cfg(feature = "USE_EXPR_LIB")]
fn SafeString_LibraryLink(
    libData: WolframLibraryData,
    Argc: mint,
    Args: *mut MArgument,
    Res: MArgument,
) -> c_int {
    if (Argc != 2) {
        return LIBRARY_FUNCTION_ERROR;
    }

    let mlSession = MArgument_getInteger(Args[0]);
    let session = mlSession as ParserSessionPtr;

    let arr = ScopedNumericArray(libData, Args[1]);

    let numBytes = arr.len();

    let data = arr.data();

    if (ParserSessionInit(
        session,
        data,
        numBytes,
        libData,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    )) {
        return LIBRARY_FUNCTION_ERROR;
    }

    let C = ParserSessionSafeString(session);

    let e = NodeContainerToExpr(session, C);

    ParserSessionReleaseNodeContainer(session, C);

    ParserSessionDeinit(session);

    MArgument_setInteger(Res, e as mint);

    return LIBRARY_NO_ERROR;
}

#[cfg(feature = "USE_MATHLINK")]
#[wll::export(wstp)]
fn SafeString_LibraryLink(link: &mut wstp::Link) {
    let len = match link.test_head("List") {
        Ok(len) => len,
        Err(err) => panic!("expected List: {err}"),
    };

    if len != 2 {
        panic!()
    }

    let _session = unsafe { read_session_from_link(link) };

    let buffer: Vec<u8> = read_input_bytes(link);

    link.new_packet().unwrap();

    let mut session = ParserSession::new(
        &buffer,
        SourceConvention::LineColumn,
        DEFAULT_TAB_WIDTH,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
        QuirkSettings::default(),
    );

    // if (ParserSessionInit(
    //     session,
    //     arr.get(),
    //     arr.getByteCount(),
    //     libData,
    //     SourceConvention::LineColumn,
    //     DEFAULT_TAB_WIDTH,
    //     FirstLineBehavior::NotScript,
    //     EncodingMode::Normal,
    // )) {
    //     return LIBRARY_FUNCTION_ERROR;
    // }

    match session.safe_string() {
        Ok(str) => link.put_str(str).unwrap(),
        Err(flag) => flag.put(link),
    };

    drop(session);
}

//======================================
// ScopedFileBuffer and related types
//======================================

// struct ScopedFileBuffer {
//     buf: MBuffer,
//     len: usize,

//     inited: bool,
//     // public:
//     //     DLLEXPORT ScopedFileBuffer(Buffer inStrIn, size_t inLen);
//     //     DLLEXPORT ~ScopedFileBuffer();
//     //     DLLEXPORT Buffer getBuf() const;
//     //     DLLEXPORT size_t getLen() const;
//     //     DLLEXPORT bool fail() const;
// }

#[cfg(feature = "USE_EXPR_LIB")]
struct ScopedNumericArray {
    libData: WolframLibraryData,
    arr: MNumericArray,
    // public:
    //     ScopedNumericArray(WolframLibraryData libData, MArgument Arg);

    //     ~ScopedNumericArray();

    //     size_t size() const;

    //     Buffer data() const;
}

// impl ScopedFileBuffer {
//     fn new(inStrIn: Buffer, inLen: isize) /* PRE_COMMIT: : buf(), len(), inited(false) */
//     {
//         unimplemented!()

//         // let inStr = reinterpret_cast<const char *>(inStrIn);
//         // let inStr: *const char = inStrIn;

//         // FILE *file = fopen(inStr, "rb");

//         // if (!file) {
//         //     return;
//         // }

//         // if (fseek(file, 0, SEEK_END)) {
//         //     return;
//         // }

//         // let res = ftell(file);

//         // if (res < 0) {
//         //     return;
//         // }
//         // len = res;

//         // rewind(file);

//         // buf = new unsigned char[len];

//         // inited = true;

//         // let r = fread(buf, sizeof(unsigned char), len, file);

//         // if (r != len) {

//         //     inited = false;

//         //     delete[] buf;
//         // }

//         // fclose(file);
//     }

//     fn getBuf(&self) -> Buffer {
//         return self.buf;
//     }

//     fn getLen(&self) -> usize {
//         return self.len;
//     }

//     fn fail(&self) -> bool {
//         return !self.inited;
//     }
// }

// impl Drop for ScopedFileBuffer {
//     fn drop(&mut self) {
//         if !self.inited {
//             return;
//         }

//         // delete[] buf;
//         delete(self.buf)
//     }
// }

// if #[cfg(feature = "USE_EXPR_LIB")] {
//     ScopedNumericArray::ScopedNumericArray(libData: WolframLibraryData, Arg: MArgument) : libData(libData), arr() {
//         arr = MArgument_getMNumericArray(Arg);
//     }

//     ScopedNumericArray::~ScopedNumericArray() {
//         libData->numericarrayLibraryFunctions->MNumericArray_disown(arr);
//     }

//     size_t ScopedNumericArray::size() const {
//         return libData->numericarrayLibraryFunctions->MNumericArray_getFlattenedLength(arr);
//     }

//     Buffer ScopedNumericArray::data() const {
//         return reinterpret_cast<Buffer>(libData->numericarrayLibraryFunctions->MNumericArray_getData(arr));
//     }
// }

/// Does the file currently have permission to be read?
#[cfg(feature = "USE_MATHLINK")]
fn validatePath(path: &str) -> bool {
    use std::ffi::c_char;

    let cstr = std::ffi::CString::new(path).expect("unable to convert file path to CString");

    let cptr: *const c_char = cstr.as_ptr();
    let cptr = cptr as *mut c_char;

    let is_valid = unsafe { wolfram_library_link::rtl::validatePath(cptr, 'R' as c_char) } != 0;

    return is_valid;
}

//======================================
// WSTP Serialization
//======================================

//==========================================================
// WSTP Deserialization Utilities
//==========================================================

fn get_expr(link: &mut wstp::Link) -> Result<Expr, wstp::Error> {
    link.get_expr_with_resolver(&mut assume_link_print_full_symbols_resolver)
}

// TODO: Make this logic part of the wstp crate in some way.
fn parse_assuming_link_print_full_symbols(link: &mut wstp::Link) -> Vec<Expr> {
    get_args_list_impl_assuming_link_print_full_symbols(link)
        .expect("unable to parse expression arguments list from WSTP link")
}

fn assume_link_print_full_symbols_resolver(s: &str) -> Option<expr::Symbol> {
    if let Some(symbol) = expr::symbol::SymbolRef::try_new(s) {
        return Some(symbol.to_symbol());
    }

    if let Some(symbol_name) = expr::symbol::SymbolNameRef::try_new(s) {
        return Some(expr::Symbol::new(&format!("System`{symbol_name}")));
    }

    None
}

fn get_args_list_impl_assuming_link_print_full_symbols(
    link: &mut wstp::Link,
) -> Result<Vec<Expr>, wstp::Error> {
    let arg_count: usize = match link.test_head("List") {
        Ok(count) => Ok(count),
        Err(err) if err.code() == Some(wstp::sys::WSEGSEQ) => {
            link.clear_error();
            link.test_head("System`List")
        },
        Err(err) => Err(err),
    }?;

    let mut elements: Vec<Expr> = Vec::new();

    for _ in 0..arg_count {
        let elem = link.get_expr_with_resolver(&mut assume_link_print_full_symbols_resolver)?;
        elements.push(elem);
    }

    Ok(elements)
}

pub(crate) fn abortQ() -> bool {
    return unsafe { wolfram_library_link::rtl::AbortQ() } != 0;
}

mod feature {
    pub const CHECK_ABORT: bool = true;
}
