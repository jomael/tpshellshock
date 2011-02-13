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

unit ExDropU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SsBase, StDrop;

type
  TMainForm = class(TForm)
    Memo: TMemo;
    ActiveCB: TCheckBox;
    ListBox: TListBox;
    TargetRG: TRadioGroup;
    StDropFiles1: TStDropFiles;
    Label1: TLabel;
    Label2: TLabel;
    TargetSLMemo: TMemo;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TargetRGClick(Sender: TObject);
    procedure ActiveCBClick(Sender: TObject);
    procedure StDropFiles1DropFiles(Sender: TObject; Point: TPoint);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  StDropFiles1.DropTarget := MainForm;
  StDropFiles1.Active := ActiveCB.Checked;
  StDropFiles1.TargetStringList := TargetSLMemo.Lines;
end;

procedure TMainForm.TargetRGClick(Sender: TObject);
begin
  case TargetRG.ItemIndex of
    0 : StDropFiles1.DropTarget := Self;
    1 : StDropFiles1.DropTarget := Memo;
    2 : StDropFiles1.DropTarget := ListBox;
  end;
end;

procedure TMainForm.ActiveCBClick(Sender: TObject);
begin
  StDropFiles1.Active := ActiveCB.Checked;
end;

procedure TMainForm.StDropFiles1DropFiles(Sender: TObject; Point: TPoint);
begin
  with StDropFiles1 do begin
    if DropTarget = Self then
      MessageDlg(IntToStr(Files.Count) +
        ' Files dropped on form.', mtInformation, [mbOk], 0);
    if DropTarget = Memo then
      Memo.Lines.Assign(Files);
    if DropTarget = ListBox then
      ListBox.Items.Assign(Files);
  end;
end;

end.
