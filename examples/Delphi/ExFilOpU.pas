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

unit ExFilOpU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StBrowsr, StdCtrls, StFileOp, ExtCtrls, SsBase;

type
  TForm1 = class(TForm)
    DstEdit: TEdit;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    StBrowser: TStBrowser;
    StFileOperation1: TStFileOperation;
    Button3: TButton;
    FileOpGroup: TRadioGroup;
    SourceMemo: TMemo;
    OpenDialog: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure StFileOperation1Error(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
    SourceMemo.Lines := OpenDialog.Files;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if StBrowser.Execute then
    DstEdit.Text := StBrowser.SelectedFolder;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  StFileOperation1.Operation := TStFileOp(FileOpGroup.ItemIndex);
  StFileOperation1.SourceFiles := SourceMemo.Lines;
  StFileOperation1.Destination := DstEdit.Text;
  StFileOperation1.Execute;
end;

procedure TForm1.StFileOperation1Error(Sender: TObject);
begin
  MessageDlg(StFileOperation1.ErrorString, mtError, [mbOk], 0);
end;

end.
