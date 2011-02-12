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
//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "ExFilOpU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StBrowsr"
#pragma link "StFileOp"
#pragma link "SsBase"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  if (OpenDialog->Execute())
    SourceMemo->Lines = OpenDialog->Files;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  if (StBrowser->Execute())
    DstEdit->Text = StBrowser->SelectedFolder;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GoButtonClick(TObject *Sender)
{
  StFileOperation1->Operation = (TStFileOp)FileOpGroup->ItemIndex;
  StFileOperation1->SourceFiles = SourceMemo->Lines;
  StFileOperation1->Destination = DstEdit->Text;
  StFileOperation1->Execute();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StFileOperation1Error(TObject *Sender)
{
  MessageDlg(StFileOperation1->ErrorString,
    mtError, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------