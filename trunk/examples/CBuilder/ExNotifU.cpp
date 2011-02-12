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

#include "ExNotifU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StShlCtl"
#pragma link "SsBase"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void TForm1::SetFlags()
{
  TStNotifyEventsSet Events;
  if (AssociationCb->Checked)
    Events << neAssociationChange;
  if (AttributeCb->Checked)
    Events << neAttributesChange;
  if (FileCreateCb->Checked)
    Events << neFileCreate;
  if (FileDeleteCb->Checked)
    Events << neFileDelete;
  if (FileRenameCb->Checked)
    Events << neFileRename;
  if (FileChangeCb->Checked)
    Events << neFileChange;
  if (FolderCreateCb->Checked)
    Events << neFolderCreate;
  if (FolderDeleteCb->Checked)
    Events << neFolderDelete;
  if (FolderRenameCb->Checked)
    Events << neFolderRename;
  if (FolderChangeCb->Checked)
    Events << neFolderUpdate;
  if (DriveAddCb->Checked)
    Events << neDriveAdd;
  if (DriveRemoveCb->Checked)
    Events << neDriveRemove;
  if (ShellDriveAddCb->Checked)
    Events << neShellDriveAdd;
  if (NetShareCb->Checked)
    Events << neNetShare;
  if (NetUnShareCb->Checked)
    Events << neNetUnShare;
  if (ServerDisconnectCb->Checked)
    Events << neServerDisconnect;
  if (MediaInsertCb->Checked)
    Events << neMediaInsert;
  if (MediaRemoveCb->Checked)
    Events << neMediaRemove;
  if (FreeSpaceCb->Checked)
    Events << neDriveSpaceChange;
  if (ImageListChangeCb->Checked)
    Events << neImageListChange;
  StShellNotification1->NotifyEvents = Events;
}

void __fastcall TForm1::ActiveCbClick(TObject *Sender)
{
  if (ActiveCb->Checked) {
    TStShellFolder* SF = StShellTreeView1->SelectedFolder;
    if (SF == 0) {
      ShowMessage("No folder selected.");
      return;
    }
    if (SF->IsFileFolder)
      StShellNotification1->WatchFolder = SF->Path;
    else
      StShellNotification1->WatchPidl = SF->Pidl;
    StShellNotification1->WatchSubFolders = WatchSubFoldersCb->Checked;
    StShellNotification1->NotifyEvents.Clear();
    SetFlags();
    StShellNotification1->Active = true;
  } else
   StShellNotification1->Active = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::EventItemClick(TObject *Sender)
{
  SetFlags();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellTreeView1FolderSelected(TObject *Sender,
      TStShellFolder *Folder)
{
  Label2->Caption = "Notification events for " +
    Folder->DisplayName;
  ActiveCbClick(0);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1AssociationChange(
      TObject *Sender)
{
  Memo1->Lines->Add("Association change");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1AttributeChange(
      TObject *Sender, TStShellItem *OldShellItem,
      TStShellItem *NewShellItem)
{
  Memo1->Lines->Add("Attribute change");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1DriveAdd(TObject *Sender,
      TStShellItem *ShellItem)
{
  Memo1->Lines->Add("New drive added: " + ShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1DriveRemove(TObject *Sender,
      TStShellItem *ShellItem)
{
  Memo1->Lines->Add("Drive removed: " + ShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1DriveSpaceChange(
      TObject *Sender, int Drive)
{
  Memo1->Lines->Add("Drive space changed");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1FileChange(TObject *Sender,
      TStShellItem *ShellItem)
{
  Memo1->Lines->Add("File changed: " + ShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1FileCreate(TObject *Sender,
      TStShellItem *ShellItem)
{
  Memo1->Lines->Add("New file created: " + ShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1FileDelete(TObject *Sender,
      TStShellItem *OldShellItem, TStShellItem *NewShellItem)
{
  Memo1->Lines->Add("File deleted: " + OldShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1FileRename(TObject *Sender,
      TStShellItem *OldShellItem, TStShellItem *NewShellItem)
{
  Memo1->Lines->Add("File renamed from " +
    OldShellItem->DisplayName + " to " + NewShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1FolderChange(TObject *Sender,
      TStShellItem *ShellItem)
{
  Memo1->Lines->Add("Folder changed: " + ShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1FolderCreate(TObject *Sender,
      TStShellItem *ShellItem)
{
  Memo1->Lines->Add("New folder created: " + ShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1FolderDelete(TObject *Sender,
      TStShellItem *ShellItem)
{
  Memo1->Lines->Add("Folder deleted: " + ShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1FolderRename(TObject *Sender,
      TStShellItem *OldShellItem, TStShellItem *NewShellItem)
{
  Memo1->Lines->Add("Folder renamed from " +
    OldShellItem->DisplayName + " to " + NewShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1ImageListChange(
      TObject *Sender)
{
  Memo1->Lines->Add("Image list changed");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1MediaInsert(TObject *Sender,
      TStShellItem *ShellItem)
{
  Memo1->Lines->Add("Media inserted into " + ShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1MediaRemove(TObject *Sender,
      TStShellItem *ShellItem)
{
  Memo1->Lines->Add("Media removed from " + ShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1NetShare(TObject *Sender,
      TStShellItem *ShellItem)
{
  Memo1->Lines->Add("Network share added: " + ShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1NetUnShare(TObject *Sender,
      TStShellItem *ShellItem)
{
  Memo1->Lines->Add("Network share removed: " + ShellItem->DisplayName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StShellNotification1ServerDisconnect(
      TObject *Sender, TStShellItem *ShellItem)
{
  Memo1->Lines->Add("Server disconnected: " + ShellItem->DisplayName);
}
//---------------------------------------------------------------------------
