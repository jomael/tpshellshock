TurboPower ShellShock


Table of contents

1.  Introduction
2.  Package names
3.  Installation
4.  Version history
4.1   Release 1.02

==============================================


1. Introduction

ShellShock provides a set of components that let you customize
applications with the functionality available in the Windows Shell &
Windows Explorer, all without writing code. The components are written
in native VCL for Borland Delphi & C++Builder.

This is a source-only release of TurboPower ShellShock. It includes
designtime and runtime packages for Delphi 3 through 7 and C++Builder
3 through 6.

For help files and a PDF manual, please see the tpshellshock_docs
package on SourceForge (http://sourceforge.net/projects/tpshellshock).

==============================================

2. Package names


TurboPower ShellShock package names have the following form:

  KNNNMKVV.*
   |  |||
   |  ||+------ VV  VCL version (30=Delphi 3, 40=Delphi 4, 70=Delphi 7)
   |  |+------- K   Kind of package (R=runtime, D=designtime)
   |  +-------- M   Product-specific modifier (typically underscore)
   +----------- NNN Product version number (e.g., 102=version 1.02)


For example, the ShellShock designtime package files for Delphi 7 have
the filename K102_D70.*.

The runtime package contains the core functionality of the product and
is not installed into the IDE. The designtime package references the
runtime package, registers the components, and contains property
editors used in the IDE.

==============================================

3. Installation


To install TurboPower ShellShock into your IDE, take the following
steps:

  1. Unzip the release files into a directory (e.g., d:\shellshock).

  2. Start Delphi or C++Builder.

  3. Add the source subdirectory (e.g., d:\shellshock\source) to the
     IDE's library path.

  4. Open & compile the runtime package specific to the IDE being
     used.

  5. Open & install the designtime package specific to the IDE being
     used. The IDE should notify you the components have been
     installed.

==============================================

4. Version history


4.1 Release 1.02

Bugs Fixed
----------

- Stretched icons in "Large Fonts" mode

Enhancements
------------

- Delphi 7 support
