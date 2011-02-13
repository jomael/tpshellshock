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

unit ExExplU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StShlCtl, ExtCtrls, StdCtrls, SsBase, StFileOp, Menus,
  ToolWin, {$IFDEF VERSION5} ImgList,{$ENDIF} Clipbrd, ImgList;

type
  TForm1 = class(TForm)
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    StShellTreeView: TStShellTreeView;
    StShellListView: TStShellListView;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Properties1: TMenuItem;
    Rename1: TMenuItem;
    Delete1: TMenuItem;
    N3: TMenuItem;
    New1: TMenuItem;
    ToolBar1: TToolBar;
    StShellComboBox: TStShellComboBox;
    Edit1: TMenuItem;
    Paste1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    ImageList: TImageList;
    ToolButton1: TToolButton;
    MoveUpBtn: TToolButton;
    N2: TMenuItem;
    CopyPath1: TMenuItem;
    procedure StShellListViewListFilled(Sender: TObject);
    procedure StShellListViewItemSelected(Sender: TObject;
      Item: TStShellItem);
    procedure New1Click(Sender: TObject);
    procedure Properties1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure CopyPath1Click(Sender: TObject);
    procedure StShellTreeViewKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StShellListViewKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.StShellListViewListFilled(Sender: TObject);
var
  S : string;
begin
  S := IntToStr(StShellListView.ShellItems.Count) + ' object(s)';
  if not (loShowHidden in StShellListView.Options) then
    if StShellListView.Folder.IsFileSystem and
        (StShellListView.Folder.HiddenCount <> 0) then
      S := S + ' (plus ' + IntToStr(
        StShellListView.Folder.HiddenCount) + ' hidden)';
  StatusBar1.Panels[0].Text := S;
end;

procedure TForm1.StShellListViewItemSelected(Sender: TObject;
  Item: TStShellItem);
begin
  StatusBar1.Panels[1].Text := Item.Path;
end;

procedure TForm1.New1Click(Sender: TObject);
begin
  StShellListView.AddFolder('');
end;

procedure TForm1.Properties1Click(Sender: TObject);
begin
  if ActiveControl = StShellListView then
    StShellListView.ShowPropertySheet
  else if ActiveControl = StShellTreeView then
    StShellTreeView.ShowPropertySheet;
end;

procedure TForm1.Cut1Click(Sender: TObject);
begin
  if ActiveControl = StShellListView then
    StShellListView.CutToClipboard
  else if ActiveControl = StShellTreeView then
    StShellTreeView.CutToClipboard;
end;

procedure TForm1.Copy1Click(Sender: TObject);
begin
  if ActiveControl = StShellListView then
    StShellListView.CopyToClipboard
  else if ActiveControl = StShellTreeView then
    StShellTreeView.CopyToClipboard;
end;

procedure TForm1.Paste1Click(Sender: TObject);
begin
  if ActiveControl = StShellListView then
    StShellListView.PasteFromClipboard
  else if ActiveControl = StShellTreeView then
    StShellTreeView.PasteFromClipboard;
end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
  if ActiveControl = StShellListView then begin
    Cut1.Enabled := StShellListView.SelectedItem.CanCopy;
    Copy1.Enabled := Cut1.Enabled;
    CopyPath1.Enabled := Cut1.Enabled;
    Paste1.Enabled := StShellListView.SelectedItem.CanPaste;
  end else if (ActiveControl = StShellTreeView) and
      Assigned(StShellTreeView.SelectedFolder) then begin
    Cut1.Enabled := StShellTreeView.SelectedFolder.CanCopy;
    Copy1.Enabled := Cut1.Enabled;
    CopyPath1.Enabled := Cut1.Enabled;
    Paste1.Enabled := StShellTreeView.SelectedFolder.CanPaste;
  end else begin
    Cut1.Enabled := False;
    Copy1.Enabled := False;
    Paste1.Enabled := False;
    CopyPath1.Enabled := False;
  end;
end;

procedure TForm1.File1Click(Sender: TObject);
begin
  if ActiveControl = StShellListView then
    Properties1.Enabled := StShellListView.SelectedItem.HasPropSheet
  else if (ActiveControl = StShellTreeView) and
       Assigned(StShellTreeView.SelectedFolder) then begin
    Properties1.Enabled := StShellTreeView.SelectedFolder.HasPropSheet
  end else
    Properties1.Enabled := False;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.MoveUpBtnClick(Sender: TObject);
begin
  StShellListView.MoveUpOneLevel;
end;

procedure TForm1.CopyPath1Click(Sender: TObject);
begin
  if ActiveControl = StShellListView then
    Clipboard.AsText := StShellListView.SelectedItem.Path
  else if ActiveControl = StShellTreeView then
    Clipboard.AsText := StShellTreeView.SelectedFolder.Path;
end;

procedure TForm1.StShellTreeViewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F5 :
      StShellTreeView.Refresh(StShellTreeView.Selected);
    VK_DELETE :
      if ssShift in Shift then
        StShellTreeView.DeleteFolder(False, True)
      else
        StShellTreeView.DeleteFolder(True, True);
    VK_F2 :
      StShellTreeView.Selected.EditText;
    VK_F4 :
      begin
        StShellComboBox.DroppedDown := True;
        StShellComboBox.SetFocus;
      end;
  end;
end;

procedure TForm1.StShellListViewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F5 :
      StShellListView.Refresh;
    VK_DELETE :
      if ssShift in Shift then
        StShellListView.DeleteItem(False, True)
      else
        StShellListView.DeleteItem(True, True);
    VK_F2 :
      StShellListView.Selected.EditCaption;
    VK_F4 :
      begin
        StShellComboBox.DroppedDown := True;
        StShellComboBox.SetFocus;
      end;
  end;
end;

end.

