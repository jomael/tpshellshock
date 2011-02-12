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
#ifndef ExSCutUH
#define ExSCutUH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "StShrtCt.hpp"
#include <vcl\Dialogs.hpp>
#include "SsBase.hpp"
//---------------------------------------------------------------------------
class PACKAGE TMainForm : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label1;
	TButton *CreateBtn;
	TMemo *Memo;
	TButton *OpenBtn;
	TStShortcut *StShortcut1;
	TOpenDialog *OpenDialog;
	void __fastcall OpenBtnClick(TObject *Sender);
	void __fastcall CreateBtnClick(TObject *Sender);
private:	// User declarations
  void Resolve();
public:		// User declarations
	__fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
