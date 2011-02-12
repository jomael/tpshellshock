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

unit OpenDlgU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, StShlCtl, Buttons, ExtCtrls, CommCtrl, SsBase;

type
  TStOpenDialog = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    MoveUpBtn: TSpeedButton;
    NewFolderBtn: TSpeedButton;
    ListBtn: TSpeedButton;
    DetailsBtn: TSpeedButton;
    StShellListView1: TStShellListView;
    StShellComboBox1: TStShellComboBox;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    FileNameEd: TEdit;
    TypeCb: TComboBox;
    OpenBtn: TButton;
    CancelBtn: TButton;
    Notebook1: TNotebook;
    Label5: TLabel;
    PreviewMemo: TMemo;
    PreviewImage: TImage;
    Panel6: TPanel;
    Splitter1: TSplitter;
    procedure StShellListView1ItemSelected(Sender: TObject;
      Item: TStShellItem);
    procedure ListBtnClick(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure StShellListView1DblClick(Sender: TObject);
    procedure NewFolderBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure TypeCbChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StOpenDialog: TStOpenDialog;

implementation

{$R *.DFM}

procedure TStOpenDialog.StShellListView1ItemSelected(Sender: TObject;
  Item: TStShellItem);
var
  S : string;
begin
  if not Item.IsFolder then
    FileNameEd.Text := Item.DisplayName;
  if FileExists(Item.Path) then begin
    S := LowerCase(ExtractFileExt(Item.Path));
    if S <> '' then begin
      if (S = '.txt') or (S = '.bat') or (S = '.pas') or
         (S = '.cpp') or (S = '.c') or (S = '.h') or (S = '.hpp') then begin
        Notebook1.PageIndex := 2;
        PreviewMemo.Lines.LoadFromFile(Item.Path);
        Exit;
      end;
      if (S = '.bmp') or (S = '.wmf') or (S = '.ico') then begin
        Notebook1.PageIndex := 3;
        try
          PreviewImage.Picture.LoadFromFile(Item.Path);
          PreviewImage.Top := (Notebook1.Height div 2) -
            (PreviewImage.Height div 2);
          PreviewImage.Left := (Notebook1.Width div 2) -
            (PreviewImage.Width div 2);
        except
          Notebook1.PageIndex := 0;
        end;
        Exit;
      end;
    end;
    try
      Notebook1.PageIndex := 1;
    except
      Notebook1.PageIndex := 0;
    end;
  end;
end;

procedure TStOpenDialog.ListBtnClick(Sender: TObject);
begin
  StShellListView1.ViewStyle := vsList;
end;

procedure TStOpenDialog.DetailsBtnClick(Sender: TObject);
begin
  StShellListView1.ViewStyle := vsReport;
end;

procedure TStOpenDialog.MoveUpBtnClick(Sender: TObject);
begin
  StShellListView1.MoveUpOneLevel;
end;

procedure TStOpenDialog.StShellListView1DblClick(Sender: TObject);
begin
  if FileExists(StShellListView1.SelectedItem.Path) then
    Close;
end;

procedure TStOpenDialog.NewFolderBtnClick(Sender: TObject);
begin
  StShellListView1.AddFolder('');
end;

procedure TStOpenDialog.FormCreate(Sender: TObject);
begin
  TypeCb.ItemIndex := 0;
end;

procedure TStOpenDialog.OpenBtnClick(Sender: TObject);
var
  S : string;
begin
  S := StShellListView1.SelectedItem.Path;
  if FileExists(S) then begin
    ModalResult := mrOk;
    Exit;
  end;
  if (Pos('*', FileNameEd.Text) <> 0) or
     (Pos('?', FileNameEd.Text) <> 0) or
     (FileNameEd.Text = '') then begin
    StShellListView1.FileFilter := FileNameEd.Text;
    StShellListView1.Filtered := True;
  end else begin
    StShellListView1.Filtered := False;
    if IsDirectory(FileNameEd.Text) then begin
      StShellListView1.RootFolder := FileNameEd.Text;
      FileNameEd.SelStart := 0;
      FileNameEd.SelLength := Length(FileNameEd.Text);
    end else begin
      S := StShellListView1.Folder.Path;
      if S[Length(S)] <> '\' then
        S := S + '\';
      S := S + FileNameEd.Text;
      if IsDirectory(S) then begin
        StShellListView1.RootFolder := S;
        FileNameEd.SelStart := 0;
        FileNameEd.SelLength := Length(FileNameEd.Text);
      end;
    end;
  end;
end;

procedure TStOpenDialog.TypeCbChange(Sender: TObject);
var
  S : string;
begin
  S := TypeCb.Text;
  Delete(S, 1, Pos('(', S));
  Delete(S, Length(S), 1);
  StShellListView1.FileFilter := S;
  StShellListView1.Filtered := True;
end;

end.
