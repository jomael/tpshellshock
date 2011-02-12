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
#ifndef ExExplUH
#define ExExplUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "SsBase.hpp"
#include "StShlCtl.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
    TSplitter *Splitter1;
    TStatusBar *StatusBar1;
    TStShellTreeView *StShellTreeView;
    TStShellListView *StShellListView;
    TToolBar *ToolBar1;
    TStShellComboBox *StShellComboBox;
    TToolButton *ToolButton1;
    TToolButton *MoveUpBtn;
    TMainMenu *MainMenu;
    TMenuItem *File1;
    TMenuItem *New1;
    TMenuItem *N3;
    TMenuItem *Delete1;
    TMenuItem *Rename1;
    TMenuItem *Properties1;
    TMenuItem *N1;
    TMenuItem *Exit1;
    TMenuItem *Edit1;
    TMenuItem *Cut1;
    TMenuItem *Copy1;
    TMenuItem *Paste1;
    TMenuItem *N2;
    TMenuItem *CopyPath1;
    TImageList *ImageList;
    void __fastcall StShellListViewListFilled(TObject *Sender);
    void __fastcall StShellListViewItemSelected(TObject *Sender,
          TStShellItem *Item);
    void __fastcall New1Click(TObject *Sender);
    void __fastcall Properties1Click(TObject *Sender);
    void __fastcall Cut1Click(TObject *Sender);
    void __fastcall Copy1Click(TObject *Sender);
    void __fastcall Paste1Click(TObject *Sender);
    void __fastcall CopyPath1Click(TObject *Sender);
    void __fastcall Edit1Click(TObject *Sender);
    void __fastcall File1Click(TObject *Sender);
    void __fastcall Exit1Click(TObject *Sender);
    void __fastcall MoveUpBtnClick(TObject *Sender);
    void __fastcall StShellTreeViewKeyUp(TObject *Sender, WORD &Key,
          TShiftState Shift);
    void __fastcall StShellListViewKeyUp(TObject *Sender, WORD &Key,
          TShiftState Shift);
private:	// User declarations
public:		// User declarations
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
