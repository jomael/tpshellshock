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

unit ExTrayU;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, SsBase,
  {$IFNDEF VER100} ImgList, {$ENDIF} StTrIcon;

type
  TMainForm = class(TForm)
    PopupMenu1: TPopupMenu;
    Test1: TMenuItem;
    Test21: TMenuItem;
    CheckBox: TCheckBox;
    StTrayIcon: TStTrayIcon;
    ImageList1: TImageList;
    AnimateCBox: TCheckBox;
    procedure RestoreClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure AnimateCBoxClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.RestoreClick(Sender: TObject);
begin
  StTrayIcon.RestoreApplication;
end;

procedure TMainForm.CloseClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.CheckBoxClick(Sender: TObject);
begin
  StTrayIcon.Active := CheckBox.Checked;
end;

procedure TMainForm.AnimateCBoxClick(Sender: TObject);
begin
  StTrayIcon.Animate := AnimateCBox.Checked;
end;

end.
