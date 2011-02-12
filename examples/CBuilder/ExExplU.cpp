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
#include <vcl.h>
#pragma hdrstop

#include "ExExplU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "SsBase"
#pragma link "StShlCtl"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellListViewListFilled(TObject *Sender)
{
  String S = String(StShellListView->ShellItems->Count) + " object(s)";
  if (!StShellListView->Options.Contains(loShowHidden))
    if (StShellListView->Folder->IsFileSystem &&
        StShellListView->Folder->HiddenCount != 0)
      S = S + " (plus " + String(
        StShellListView->Folder->HiddenCount) + " hidden)";
  StatusBar1->Panels->Items[0]->Text = S;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::StShellListViewItemSelected(TObject *Sender,
      TStShellItem *Item)
{
  StatusBar1->Panels->Items[1]->Text = Item->Path;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::New1Click(TObject *Sender)
{
  StShellListView->AddFolder("");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Properties1Click(TObject *Sender)
{
  if (ActiveControl == StShellListView)
    StShellListView->ShowPropertySheet();
  else if (ActiveControl == StShellTreeView)
    StShellTreeView->ShowPropertySheet();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Cut1Click(TObject *Sender)
{
  if (ActiveControl == StShellListView)
    StShellListView->CutToClipboard();
  else if (ActiveControl == StShellTreeView)
    StShellTreeView->CutToClipboard();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Copy1Click(TObject *Sender)
{
  if (ActiveControl == StShellListView)
    StShellListView->CopyToClipboard();
  else if (ActiveControl == StShellTreeView)
    StShellTreeView->CopyToClipboard();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Paste1Click(TObject *Sender)
{
  if (ActiveControl == StShellListView)
    StShellListView->PasteFromClipboard();
  else if (ActiveControl == StShellTreeView)
    StShellTreeView->PasteFromClipboard();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CopyPath1Click(TObject *Sender)
{
  if (ActiveControl == StShellListView)
    Clipboard()->AsText = StShellListView->SelectedItem->Path;
  else if (ActiveControl == StShellTreeView)
    Clipboard()->AsText = StShellTreeView->SelectedFolder->Path;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Edit1Click(TObject *Sender)
{
  if (ActiveControl == StShellListView) {
    Cut1->Enabled = StShellListView->SelectedItem->CanCopy;
    Copy1->Enabled = Cut1->Enabled;
    CopyPath1->Enabled = Cut1->Enabled;
    Paste1->Enabled = StShellListView->SelectedItem->CanPaste;
  } else if (ActiveControl == StShellTreeView
         && StShellTreeView->SelectedFolder) {
    Cut1->Enabled = StShellTreeView->SelectedFolder->CanCopy;
    Copy1->Enabled = Cut1->Enabled;
    CopyPath1->Enabled = Cut1->Enabled;
    Paste1->Enabled = StShellTreeView->SelectedFolder->CanPaste;
  } else {
    Cut1->Enabled = false;
    Copy1->Enabled = false;
    Paste1->Enabled = false;
    CopyPath1->Enabled = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::File1Click(TObject *Sender)
{
  if (ActiveControl == StShellListView)
    Properties1->Enabled = StShellListView->SelectedItem->HasPropSheet;
  else if (ActiveControl == StShellTreeView)
    Properties1->Enabled = StShellTreeView->SelectedFolder->HasPropSheet;
  else
    Properties1->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Exit1Click(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MoveUpBtnClick(TObject *Sender)
{
  StShellListView->MoveUpOneLevel();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellTreeViewKeyUp(TObject *Sender, WORD &Key,
      TShiftState Shift)
{
  switch (Key) {
    case VK_F5 :
      StShellTreeView->Refresh(StShellTreeView->Selected);
      break;
    case VK_DELETE :
      if (Shift.Contains(ssShift))
        StShellTreeView->DeleteFolder(false, true);
      else
        StShellTreeView->DeleteFolder(true, true);
      break;
    case VK_F2 :
      StShellTreeView->Selected->EditText();
      break;
    case VK_F4 :
      StShellComboBox->DroppedDown = true;
      StShellComboBox->SetFocus();
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::StShellListViewKeyUp(TObject *Sender, WORD &Key,
      TShiftState Shift)
{
  switch (Key) {
    case VK_F5 :
      StShellListView->Refresh();
      break;
    case VK_DELETE :
      if (Shift.Contains(ssShift))
        StShellListView->DeleteItem(false, true);
      else
        StShellListView->DeleteItem(true, true);
      break;
    case VK_F2 :
      StShellListView->Selected->EditCaption();
      break;
    case VK_F4 :
      StShellComboBox->DroppedDown = true;
      StShellComboBox->SetFocus();
  }
}
//---------------------------------------------------------------------------

