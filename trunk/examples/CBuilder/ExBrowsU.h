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
#ifndef ExBrowsUH
#define ExBrowsUH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "StBrowsr.hpp"
#include "StAbout.hpp"
#include "SsBase.hpp"
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
	TButton *AboutBtn;
	TGroupBox *GroupBox1;
	TLabel *Label1;
	TLabel *DisplayNameLabel;
	TLabel *Label2;
	TLabel *PathLabel;
	TLabel *Label3;
	TLabel *ImageIndexLabel;
	TLabel *Label4;
	TLabel *SelectedFolderLabel;
	TLabel *Label5;
	TLabel *Label6;
	TLabel *Label7;
	TButton *BrowseBtn;
	TGroupBox *GroupBox2;
	TCheckBox *ComputerCB;
	TCheckBox *PrinterCB;
	TCheckBox *DomainCB;
	TCheckBox *AncestorCB;
	TCheckBox *DirectoriesCB;
	TCheckBox *ShowFilesCB;
	TCheckBox *ShowEditCB;
	TEdit *RootEdit;
	TEdit *SelFolderEdit;
	TComboBox *StartFolderCombo;
	TStBrowser *StBrowser1;
	TStShellAbout *StShellAbout1;
	void __fastcall AboutBtnClick(TObject *Sender);
	void __fastcall BrowseBtnClick(TObject *Sender);
	void __fastcall StBrowser1SelChanged(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	
private:	// User declarations
public:		// User declarations
	__fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
