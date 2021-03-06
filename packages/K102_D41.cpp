// ***** BEGIN LICENSE BLOCK *****
// * Version: MPL 1.1
// *
// * The contents of this file are subject to the Mozilla Public License Version
// * 1.1 (the "License"); you may not use this file except in compliance with
// * the License. You may obtain a copy of the License at
// * http://www.mozilla.org/MPL/
// *
// * Software distributed under the License is distributed on an "AS IS" basis,
// * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// * for the specific language governing rights and limitations under the
// * License.
// *
// * The Original Code is TurboPower ShellShock
// *
// * The Initial Developer of the Original Code is
// * TurboPower Software
// *
// * Portions created by the Initial Developer are Copyright (C) 1996-2002
// * the Initial Developer. All Rights Reserved.
// *
// * Contributor(s):
// *
// * ***** END LICENSE BLOCK *****
/*********************************************************/
/* ShellShock: K102_D41.CPP 1.02                         */
/*********************************************************/

//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("K102_D41.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("..\source\SsPropEd.pas");
USEUNIT("..\source\SsNotif0.pas");
USEFORMNS("..\source\SsAbout0.pas", Ssabout0, SsAboutForm);
USEUNIT("..\source\SsReg.pas");
USEFORMNS("..\source\SsFilter0.pas", Ssfilter0, FilterEdForm);
USEPACKAGE("K102_R41.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
