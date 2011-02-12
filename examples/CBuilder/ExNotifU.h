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
#ifndef ExNotifUH
#define ExNotifUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "StShlCtl.hpp"
#include <ComCtrls.hpp>
#include "SsBase.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label2;
  TGroupBox *GroupBox1;
  TCheckBox *AssociationCb;
  TCheckBox *AttributeCb;
  TCheckBox *FileCreateCb;
  TCheckBox *FileDeleteCb;
  TCheckBox *FileRenameCb;
  TCheckBox *FolderRenameCb;
  TCheckBox *FolderChangeCb;
  TCheckBox *FileChangeCb;
  TCheckBox *FolderCreateCb;
  TCheckBox *FolderDeleteCb;
  TCheckBox *DriveAddCb;
  TCheckBox *DriveRemoveCb;
  TCheckBox *ShellDriveAddCb;
  TCheckBox *NetShareCb;
  TCheckBox *NetUnShareCb;
  TCheckBox *FreeSpaceCb;
  TCheckBox *ImageListChangeCb;
  TCheckBox *ServerDisconnectCb;
  TCheckBox *MediaInsertCb;
  TCheckBox *MediaRemoveCb;
  TMemo *Memo1;
  TGroupBox *GroupBox2;
  TLabel *Label1;
  TCheckBox *ActiveCb;
  TCheckBox *WatchSubFoldersCb;
  TStShellTreeView *StShellTreeView1;
  TStShellNotification *StShellNotification1;
  void __fastcall ActiveCbClick(TObject *Sender);
  void __fastcall EventItemClick(TObject *Sender);
  void __fastcall StShellTreeView1FolderSelected(TObject *Sender,
          TStShellFolder *Folder);
  void __fastcall StShellNotification1AssociationChange(TObject *Sender);
  void __fastcall StShellNotification1AttributeChange(TObject *Sender,
          TStShellItem *OldShellItem, TStShellItem *NewShellItem);
  void __fastcall StShellNotification1DriveAdd(TObject *Sender,
          TStShellItem *ShellItem);
  void __fastcall StShellNotification1DriveRemove(TObject *Sender,
          TStShellItem *ShellItem);
  void __fastcall StShellNotification1DriveSpaceChange(TObject *Sender,
          int Drive);
  void __fastcall StShellNotification1FileChange(TObject *Sender,
          TStShellItem *ShellItem);
  void __fastcall StShellNotification1FileCreate(TObject *Sender,
          TStShellItem *ShellItem);
  void __fastcall StShellNotification1FileDelete(TObject *Sender,
          TStShellItem *OldShellItem, TStShellItem *NewShellItem);
  void __fastcall StShellNotification1FileRename(TObject *Sender,
          TStShellItem *OldShellItem, TStShellItem *NewShellItem);
  void __fastcall StShellNotification1FolderChange(TObject *Sender,
          TStShellItem *ShellItem);
  void __fastcall StShellNotification1FolderCreate(TObject *Sender,
          TStShellItem *ShellItem);
  void __fastcall StShellNotification1FolderDelete(TObject *Sender,
          TStShellItem *ShellItem);
  void __fastcall StShellNotification1FolderRename(TObject *Sender,
          TStShellItem *OldShellItem, TStShellItem *NewShellItem);
  void __fastcall StShellNotification1ImageListChange(TObject *Sender);
  void __fastcall StShellNotification1MediaInsert(TObject *Sender,
          TStShellItem *ShellItem);
  void __fastcall StShellNotification1MediaRemove(TObject *Sender,
          TStShellItem *ShellItem);
  void __fastcall StShellNotification1NetShare(TObject *Sender,
          TStShellItem *ShellItem);
  void __fastcall StShellNotification1NetUnShare(TObject *Sender,
          TStShellItem *ShellItem);
  void __fastcall StShellNotification1ServerDisconnect(TObject *Sender,
          TStShellItem *ShellItem);
  

private:	// User declarations
  void SetFlags();
public:		// User declarations
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
