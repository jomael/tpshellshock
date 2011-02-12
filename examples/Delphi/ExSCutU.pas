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

unit ExSCutU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SsBase, StShrtCt, StdCtrls;

type
  TForm1 = class(TForm)
    StShortcut1: TStShortcut;
    CreateBtn: TButton;
    Memo: TMemo;
    Label1: TLabel;
    OpenBtn: TButton;
    OpenDialog: TOpenDialog;
    procedure CreateBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure Resolve;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.CreateBtnClick(Sender: TObject);
begin
  if not StShortcut1.CreateShortcut then
    MessageDlg('Error creating shortcut.', mtError, [mbOk], 0)
  else
    MessageDlg('Shortcut created!', mtError, [mbOk], 0);
end;

procedure TForm1.OpenBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    if UpperCase(ExtractFileExt(OpenDialog.FileName)) = UpperCase('.LNK') then begin
      StShortCut1.ShortcutFileName := OpenDialog.FileName;
      CreateBtn.Enabled := False;
      Resolve;
    end else begin
      StShortcut1.FileName := OpenDialog.FileName;
      CreateBtn.Enabled := True;
    end;
end;

procedure TForm1.Resolve;
begin
  StShortcut1.ShortcutFileName := OpenDialog.FileName;
  if not StShortcut1.ResolveShortcut then
    MessageDlg('Error resolving shortcut', mtError, [mbOk], 0)
  else with Memo, StShortcut1 do begin
    Clear;
    Lines.Add('Target File: ' + FileName);
    Lines.Add('Start In Directory: ' + StartInDir);
    Lines.Add('Description: ' + Description);
    Lines.Add('Show Command: ' + IntToStr(Ord(ShowCommand)));
  end;
end;

end.
