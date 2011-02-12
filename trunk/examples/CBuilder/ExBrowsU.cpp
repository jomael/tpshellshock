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

#include "ExBrowsU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StAbout"
#pragma link "SsBase"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::AboutBtnClick(TObject *Sender)
{
  StShellAbout1->Execute();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::BrowseBtnClick(TObject *Sender)
{
  TStBrowseOptionsSet Options;
  Options.Clear();
  if (ComputerCB->Checked)
    Options << boBrowseForComputer;
  if (PrinterCB->Checked)
    Options << boBrowseForPrinter;
  if (DomainCB->Checked)
    Options << boDontGoBelowDomain;
  if (AncestorCB->Checked)
    Options << boReturnOnlyAncestors;
  if (DirectoriesCB->Checked)
    Options << boReturnOnlyDirs;
  if (ShowFilesCB->Checked)
    Options << boShowFiles;
  if (ShowEditCB->Checked)
    Options << boEditBox;
  StBrowser1->Options = Options;
  StBrowser1->RootFolder = RootEdit->Text;
  StBrowser1->SelectedFolder = SelFolderEdit->Text;
  if (StartFolderCombo->ItemIndex == -1 || StartFolderCombo->Text == "(None)")
    StBrowser1->SpecialRootFolder = sfNone;
  else
    StBrowser1->SpecialRootFolder = (TStSpecialRootFolder)
    (StartFolderCombo->Items->IndexOf(StartFolderCombo->Text) - 1);
  if (StBrowser1->Execute()) {
    DisplayNameLabel->Caption = StBrowser1->DisplayName;
    PathLabel->Caption = StBrowser1->Path;
    ImageIndexLabel->Caption = IntToStr(StBrowser1->ImageIndex);
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::StBrowser1SelChanged(TObject *Sender)
{
  if (UpperCase(StBrowser1->SelectedFolder) == UpperCase("d:\\"))
    StBrowser1->OKEnabled = False;
  SelectedFolderLabel->Caption = StBrowser1->SelectedFolder;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject *Sender)
{
  StartFolderCombo->Text = StartFolderCombo->Items->Strings[0];
}
//---------------------------------------------------------------------------
