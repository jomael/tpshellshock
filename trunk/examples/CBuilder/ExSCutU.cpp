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

#include "ExSCutU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StShrtCt"
#pragma link "SsBase"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::OpenBtnClick(TObject *Sender)
{
  if (OpenDialog->Execute())
    if (UpperCase(ExtractFileExt(OpenDialog->FileName)) == UpperCase(".LNK") ) {
      StShortcut1->ShortcutFileName = OpenDialog->FileName;
      CreateBtn->Enabled = False;
      Resolve();
    }
    else {
      StShortcut1->FileName = OpenDialog->FileName;
      CreateBtn->Enabled = True;
    }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::CreateBtnClick(TObject *Sender)
{
  if (!StShortcut1->CreateShortcut())
    MessageDlg("Error creating shortcut.", mtError, TMsgDlgButtons() << mbOK, 0);
  else
    MessageDlg("Shortcut created!", mtError, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------
void TMainForm::Resolve()
{
  StShortcut1->ShortcutFileName = OpenDialog->FileName;
  if (!StShortcut1->ResolveShortcut())
    MessageDlg("Error resolving shortcut.", mtError, TMsgDlgButtons() << mbOK, 0);
  else {
    Memo->Clear();
    Memo->Lines->Add("Target File: " + StShortcut1->FileName);
    Memo->Lines->Add("Start In Directory: " + StShortcut1->StartInDir);
    Memo->Lines->Add("Description: " + StShortcut1->Description);
    Memo->Lines->Add("Show Command: " + IntToStr(int(StShortcut1->ShowCommand)));
  }
}
