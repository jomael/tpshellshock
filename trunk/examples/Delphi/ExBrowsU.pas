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

unit ExBrowsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, StAbout, StBrowsr, ComCtrls, StFormat, SsBase;

type
  TMainForm = class(TForm)
    AboutBtn: TButton;
    StShellAbout1: TStShellAbout;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    BrowseBtn: TButton;
    StBrowser1: TStBrowser;
    DisplayNameLabel: TLabel;
    Label2: TLabel;
    PathLabel: TLabel;
    Label3: TLabel;
    ImageIndexLabel: TLabel;
    GroupBox2: TGroupBox;
    ComputerCB: TCheckBox;
    PrinterCB: TCheckBox;
    DomainCB: TCheckBox;
    AncestorCB: TCheckBox;
    DirectoriesCB: TCheckBox;
    Label4: TLabel;
    SelectedFolderLabel: TLabel;
    RootEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    SelFolderEdit: TEdit;
    Label7: TLabel;
    StartFolderCombo: TComboBox;
    ShowFilesCB: TCheckBox;
    ShowEditCB: TCheckBox;
    procedure AboutBtnClick(Sender: TObject);
    procedure BrowseBtnClick(Sender: TObject);
    procedure StBrowser1SelChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  Continue : Boolean;

implementation

{$R *.DFM}

procedure TMainForm.AboutBtnClick(Sender: TObject);
begin
  StShellAbout1.Execute;
end;

procedure TMainForm.BrowseBtnClick(Sender: TObject);
var
  Opt : TStBrowseOptionsSet;
begin
  Opt := [];
  if ComputerCB.Checked then
    Opt := Opt + [boBrowseForComputer];
  if PrinterCB.Checked then
    Opt := Opt + [boBrowseForPrinter];
  if DomainCB.Checked then
    Opt := Opt + [boDontGoBelowDomain];
  if AncestorCB.Checked then
    Opt := Opt + [boReturnOnlyAncestors];
  if DirectoriesCB.Checked then
    Opt := Opt + [boReturnOnlyDirs];
  if ShowFilesCB.Checked then
    Opt := Opt + [boShowFiles];
  if ShowEditCB.Checked then
    Opt := Opt + [boEditBox];
  with StBrowser1 do begin
    Options := Opt;
    RootFolder := RootEdit.Text;
    SelectedFolder := SelFolderEdit.Text;
    if (StartFolderCombo.ItemIndex = -1) or (StartFolderCombo.Text = '(None)') then
      SpecialRootFolder := sfNone
    else
      SpecialRootFolder := TStSpecialRootFolder(
        StartFolderCombo.Items.IndexOf(StartFolderCombo.Text) + 1);
    if Execute then begin
      DisplayNameLabel.Caption := DisplayName;
      PathLabel.Caption := Path;
      ImageIndexLabel.Caption := IntToStr(ImageIndex);
    end;
  end;
end;

procedure TMainForm.StBrowser1SelChanged(Sender: TObject);
begin
  with StBrowser1 do begin
    if UpperCase(SelectedFolder) = UpperCase('d:\') then
      OKEnabled := False;
    SelectedFolderLabel.Caption := SelectedFolder;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  StartFolderCombo.Text := StartFolderCombo.Items[0];
end;

end.
