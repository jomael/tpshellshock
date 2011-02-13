(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower ShellShock
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I SsDefine.inc} 

unit ExNotifU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, StShlCtl, ComCtrls, SsBase;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    AssociationCb: TCheckBox;
    AttributeCb: TCheckBox;
    FileCreateCb: TCheckBox;
    FileDeleteCb: TCheckBox;
    FileRenameCb: TCheckBox;
    FolderRenameCb: TCheckBox;
    FolderChangeCb: TCheckBox;
    FileChangeCb: TCheckBox;
    FolderCreateCb: TCheckBox;
    FolderDeleteCb: TCheckBox;
    DriveAddCb: TCheckBox;
    DriveRemoveCb: TCheckBox;
    ShellDriveAddCb: TCheckBox;
    NetShareCb: TCheckBox;
    NetUnShareCb: TCheckBox;
    FreeSpaceCb: TCheckBox;
    ImageListChangeCb: TCheckBox;
    ServerDisconnectCb: TCheckBox;
    MediaInsertCb: TCheckBox;
    MediaRemoveCb: TCheckBox;
    Memo1: TMemo;
    Label2: TLabel;
    StShellNotification1: TStShellNotification;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    ActiveCb: TCheckBox;
    WatchSubFoldersCb: TCheckBox;
    StShellTreeView1: TStShellTreeView;
    procedure ActiveCbClick(Sender: TObject);
    procedure StShellNotification1AssociationChange(Sender: TObject);
    procedure AssociationCbClick(Sender: TObject);
    procedure StShellNotification1AttributeChange(Sender: TObject;
      OldShellItem, NewShellItem: TStShellItem);
    procedure StShellNotification1DriveAdd(Sender: TObject;
      ShellItem: TStShellItem);
    procedure StShellNotification1DriveRemove(Sender: TObject;
      ShellItem: TStShellItem);
{$IFDEF VERSION4}
    procedure StShellNotification1DriveSpaceChange(Sender: TObject;
     Drive : Cardinal);
{$ELSE}
    procedure StShellNotification1DriveSpaceChange(Sender: TObject;
     Drive : Integer);
{$ENDIF}
    procedure StShellNotification1FileChange(Sender: TObject;
      ShellItem: TStShellItem);
    procedure StShellNotification1FileCreate(Sender: TObject;
      ShellItem: TStShellItem);
    procedure StShellNotification1FileDelete(Sender: TObject; OldShellItem,
      NewShellItem: TStShellItem);
    procedure StShellNotification1FileRename(Sender: TObject; OldShellItem,
      NewShellItem: TStShellItem);
    procedure StShellNotification1FolderChange(Sender: TObject;
      ShellItem: TStShellItem);
    procedure StShellNotification1FolderCreate(Sender: TObject;
      ShellItem: TStShellItem);
    procedure StShellNotification1FolderDelete(Sender: TObject;
      ShellItem: TStShellItem);
    procedure StShellNotification1FolderRename(Sender: TObject;
      OldShellItem, NewShellItem: TStShellItem);
    procedure StShellNotification1ImageListChange(Sender: TObject);
    procedure StShellNotification1MediaInsert(Sender: TObject;
      ShellItem: TStShellItem);
    procedure StShellNotification1MediaRemove(Sender: TObject;
      ShellItem: TStShellItem);
    procedure StShellNotification1NetShare(Sender: TObject;
      ShellItem: TStShellItem);
    procedure StShellNotification1NetUnShare(Sender: TObject;
      ShellItem: TStShellItem);
    procedure StShellNotification1ServerDisconnect(Sender: TObject;
      ShellItem: TStShellItem);
    procedure StShellNotification1ShellDriveAdd(Sender: TObject;
      ShellItem: TStShellItem);
    procedure StShellTreeView1FolderSelected(Sender: TObject;
      Folder: TStShellFolder);
  private
    { Private declarations }
    procedure SetFlags;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.ActiveCbClick(Sender: TObject);
var
  SF : TStShellFolder;
begin
  if ActiveCb.Checked then begin
    SF := StShellTreeView1.SelectedFolder;
    if SF = nil then begin
      ShowMessage('No folder selected.');
      Exit;
    end;
    if SF.IsFileFolder then
      StShellNotification1.WatchFolder := SF.Path
    else
      StShellNotification1.WatchPidl := SF.Pidl;
    StShellNotification1.WatchSubFolders := WatchSubFoldersCb.Checked;
    StShellNotification1.NotifyEvents := [];
    SetFlags;
    StShellNotification1.Active := True;
  end else
   StShellNotification1.Active := False;
end;

procedure TForm1.AssociationCbClick(Sender: TObject);
begin
  SetFlags;
end;

procedure TForm1.StShellTreeView1FolderSelected(Sender: TObject;
  Folder: TStShellFolder);
begin
  Label2.Caption := 'Notification events for ' +
    Folder.DisplayName;
  ActiveCbClick(nil);
end;

procedure TForm1.SetFlags;
var
  Events : TStNotifyEventsSet;
begin
  Events := [];
  if AssociationCb.Checked then
    Events := Events + [neAssociationChange];
  if AttributeCb.Checked then
    Events := Events + [neAttributesChange];
  if FileCreateCb.Checked then
    Events := Events + [neFileCreate];
  if FileDeleteCb.Checked then
    Events := Events + [neFileDelete];
  if FileRenameCb.Checked then
    Events := Events + [neFileRename];
  if FileChangeCb.Checked then
    Events := Events + [neFileChange];
  if FolderCreateCb.Checked then
    Events := Events + [neFolderCreate];
  if FolderDeleteCb.Checked then
    Events := Events + [neFolderDelete];
  if FolderRenameCb.Checked then
    Events := Events + [neFolderRename];
  if FolderChangeCb.Checked then
    Events := Events + [neFolderUpdate];
  if DriveAddCb.Checked then
    Events := Events + [neDriveAdd];
  if DriveRemoveCb.Checked then
    Events := Events + [neDriveRemove];
  if ShellDriveAddCb.Checked then
    Events := Events + [neShellDriveAdd];
  if NetShareCb.Checked then
    Events := Events + [neNetShare];
  if NetUnShareCb.Checked then
    Events := Events + [neNetUnShare];
  if ServerDisconnectCb.Checked then
    Events := Events + [neServerDisconnect];
  if MediaInsertCb.Checked then
    Events := Events + [neMediaInsert];
  if MediaRemoveCb.Checked then
    Events := Events + [neMediaRemove];
  if FreeSpaceCb.Checked then
    Events := Events + [neDriveSpaceChange];
  if ImageListChangeCb.Checked then
    Events := Events + [neImageListChange];
  StShellNotification1.NotifyEvents := Events;
end;

procedure TForm1.StShellNotification1AssociationChange(Sender: TObject);
begin
  Memo1.Lines.Add('Association change');
end;

procedure TForm1.StShellNotification1AttributeChange(Sender: TObject;
  OldShellItem, NewShellItem: TStShellItem);
begin
  Memo1.Lines.Add('Attribute change');
end;

procedure TForm1.StShellNotification1DriveAdd(Sender: TObject;
  ShellItem: TStShellItem);
begin
  Memo1.Lines.Add('New drive added: ' + ShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1DriveRemove(Sender: TObject;
  ShellItem: TStShellItem);
begin
  Memo1.Lines.Add('Drive removed: ' + ShellItem.DisplayName);
end;

{$IFDEF VERSION4}
procedure TForm1.StShellNotification1DriveSpaceChange(Sender: TObject;
     Drive : Cardinal);
{$ELSE}
procedure TForm1.StShellNotification1DriveSpaceChange(Sender: TObject;
     Drive : Integer);
{$ENDIF}
begin
  Memo1.Lines.Add('Drive space changed');
end;

procedure TForm1.StShellNotification1FileChange(Sender: TObject;
  ShellItem: TStShellItem);
begin
  Memo1.Lines.Add('File changed: ' + ShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1FileCreate(Sender: TObject;
  ShellItem: TStShellItem);
begin
  Memo1.Lines.Add('New file created: ' + ShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1FileDelete(Sender: TObject;
  OldShellItem, NewShellItem: TStShellItem);
begin
  Memo1.Lines.Add('File deleted: ' + OldShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1FileRename(Sender: TObject;
  OldShellItem, NewShellItem: TStShellItem);
begin
  Memo1.Lines.Add('File renamed from ' +
    OldShellItem.DisplayName + ' to ' + NewShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1FolderChange(Sender: TObject;
  ShellItem: TStShellItem);
begin
  Memo1.Lines.Add('Folder changed: ' + ShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1FolderCreate(Sender: TObject;
  ShellItem: TStShellItem);
begin
  Memo1.Lines.Add('New folder created: ' + ShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1FolderDelete(Sender: TObject;
  ShellItem: TStShellItem);
begin
  Memo1.Lines.Add('Folder deleted: ' + ShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1FolderRename(Sender: TObject;
  OldShellItem, NewShellItem: TStShellItem);
begin
  Memo1.Lines.Add('Folder renamed from ' +
    OldShellItem.DisplayName + ' to ' + NewShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1ImageListChange(Sender: TObject);
begin
  Memo1.Lines.Add('Image list changed');
end;

procedure TForm1.StShellNotification1MediaInsert(Sender: TObject;
  ShellItem: TStShellItem);
begin
  Memo1.Lines.Add('Media inserted into ' + ShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1MediaRemove(Sender: TObject;
  ShellItem: TStShellItem);
begin
  Memo1.Lines.Add('Media removed from ' + ShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1NetShare(Sender: TObject;
  ShellItem: TStShellItem);
begin
  Memo1.Lines.Add('Network share added: ' + ShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1NetUnShare(Sender: TObject;
  ShellItem: TStShellItem);
begin
  Memo1.Lines.Add('Network share removed: ' + ShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1ServerDisconnect(Sender: TObject;
  ShellItem: TStShellItem);
begin
  Memo1.Lines.Add('Server disconnected: ' + ShellItem.DisplayName);
end;

procedure TForm1.StShellNotification1ShellDriveAdd(Sender: TObject;
  ShellItem: TStShellItem);
begin
  Memo1.Lines.Add('Drive added via shell: ' + ShellItem.DisplayName);
end;

end.
