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

#include "ExDropU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StDrop"
#pragma link "SsBase"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject *Sender)
{
  StDropFiles1->DropTarget = MainForm;
  StDropFiles1->Active = ActiveCB->Checked;
  StDropFiles1->TargetStringList = TargetSLMemo->Lines;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::TargetRGClick(TObject *Sender)
{
  switch (TargetRG->ItemIndex) {
    case 0 : StDropFiles1->DropTarget = this; break;
    case 1 : StDropFiles1->DropTarget = Memo; break;
    case 2 : StDropFiles1->DropTarget = ListBox;
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ActiveCBClick(TObject *Sender)
{
  StDropFiles1->Active = ActiveCB->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::StDropFiles1DropFiles(TObject *Sender,
	tagPOINT &Point)
{
  if (StDropFiles1->DropTarget == this) 
    MessageDlg(String(StDropFiles1->Files->Count) +
      " Files dropped on form->", mtInformation, TMsgDlgButtons() << mbOK, 0);
  if (StDropFiles1->DropTarget == Memo)
    Memo->Lines->Assign(StDropFiles1->Files);
  if (StDropFiles1->DropTarget == ListBox)
    ListBox->Items->Assign(StDropFiles1->Files);
}
//---------------------------------------------------------------------------