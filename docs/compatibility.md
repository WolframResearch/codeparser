
# Compatibility


## Source Compatibility

CodeParser has source compatibility with 11.0+


## FrontEnd Compatibility

Any source .wl files that have `(* ::Package::"Tags" *)` or `(* ::Code::Initialization::"Tags" *)` syntax may only be edited with a version 12.3+ FE 


## Runtime Compatibility

Building with Wolfram 11.0+ and running with the same version should always work.

Building and running with different Wolfram versions will not always work.

Building with the latest Wolfram version is only guaranteed to work back to Wolfram 12.1+

This is due to various issues including LibraryLink versioning and rpath changes on MacOSX.


## Earlier Versions

Wolfram versions before 12.1 must build from sources to use CodeParser.

Manually modify WolframVersion in PacletInfo.wl to allow the paclet to be used.

The message that you get when you install the paclet:
```
The paclet CodeParser was successfully installed.
```
does not necessarily mean that the paclet can be used.

Make sure that the correct WolframVersion is specified.


## LibraryLink

CodeParser uses [LibraryLink](https://reference.wolfram.com/language/guide/LibraryLink.html).

The version of LibraryLink was updated in version 12.1:

| Wolfram version | LibraryLink version |
| --------------- | ------------------- |
| 12.0            | 5                   |
| 12.1            | 6                   |

The LibraryLink version is defined in the header file WolframLibrary.h

In the [LibraryLink documentation](https://reference.wolfram.com/language/LibraryLink/tutorial/LibraryStructure.html#280210622), it is described how backwards-compatibility is not maintained:

>However, you should note that you cannot use a library built with a newer version of the header into an older version of the Wolfram Language.

So LibraryLink defines Wolfram version 12.1 as a minimum that can run with libraries built with the current Wolfram version.


## rpath (MacOSX)

CodeParser uses MathLink.

The mathlink rpath was changed in version 12.1:

| Wolfram version | mathlink rpath                                                           |
| --------------- | ------------------------------------------------------------------------ |
| 12.0            | @executable_path/../Frameworks/mathlink.framework/Versions/4.36/mathlink |
| 12.1            | @rpath/mathlink.framework/Versions/4/mathlink                            |

This means that CodeParser.dylib built with 12.1+ will not work with previous versions.

It is possible to use `install_name_tool` to change the rpath, but it is recommended to build from sources.
