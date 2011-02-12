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

unit ExShEnum1U;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StShlCtl, StdCtrls, ExtCtrls, ComCtrls, SsBase;

type
  TForm1 = class(TForm)
    OptionsRg: TRadioGroup;
    Label2: TLabel;
    ResultsLb: TListBox;
    StShellEnumerator1: TStShellEnumerator;
    SortDirCb: TCheckBox;
    StShellTreeView1: TStShellTreeView;
    Label1: TLabel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ResultsLbDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SortDirCbClick(Sender: TObject);
    procedure StShellTreeView1FolderSelected(Sender: TObject;
      Folder: TStShellFolder);
  private
    { Private declarations }
    procedure AddItems;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ResultsLb.ItemHeight := GetSystemMetrics(SM_CYSMICON);
  if SortDirCb.Checked then
    StShellEnumerator1.SortDirection := sdAscending
  else
    StShellEnumerator1.SortDirection := sdDescending;
  StShellEnumerator1.Sorted := True;
end;

procedure TForm1.ResultsLbDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  SI : TStShellItem;
  Icon: TIcon;
  {$IFNDEF VERSION5}
  BM1 : TBitmap;
  BM2 : TBitmap;
  {$ENDIF}
begin
  ResultsLb.Canvas.Pen.Color := clWindow;
  ResultsLb.Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
  { Get the TStShellItem that corresponds to this list box item. }
  SI := TStShellItem(ResultsLb.Items.Objects[Index]);
  { Get the small icon for the shell item. }
  Icon := SI.SmallIcon;
  {$IFDEF VERSION5}
  ResultsLb.Canvas.Draw(Rect.Left, Rect.Top, Icon);
  Inc(Rect.Left, GetSystemMetrics(SM_CYSMICON) + 5);
  {$ELSE}
  { Draw the icon. TIcon can only handle 32 x 32 icons in VCL }
  { versions prior to 5.0 so we'll have to draw the icons the hard way. }
  BM1 := TBitmap.Create;
  BM1.Height := Icon.Height;
  BM1.Width := Icon.Width;
  BM1.Transparent := True;
  BM1.Canvas.StretchDraw(Classes.Rect(0, 0, BM1.Width, BM1.Height), Icon);
  BM2 := TBitmap.Create;
  BM2.Height := GetSystemMetrics(SM_CYSMICON);
  BM2.Width := GetSystemMetrics(SM_CXSMICON);
  BM2.Transparent := True;
  BM2.Canvas.StretchDraw(Classes.Rect(0, 0, BM2.Width, BM2.Height), BM1);
  ResultsLb.Canvas.Draw(Rect.Left, Rect.Top, BM2);
  { Text will be drawn 5 pixels to the right of the icon. }
  Inc(Rect.Left, BM2.Width + 5);
  BM1.Free;
  BM2.Free;
  {$ENDIF}
  { Draw the text. }
  DrawText(ResultsLb.Canvas.Handle,
    PChar(ResultsLb.Items[Index]), -1, Rect,
    DT_VCENTER or DT_SINGLELINE);
end;

procedure TForm1.SortDirCbClick(Sender: TObject);
begin
  { Set the value of the SortDirection property based }
  { on the value of the check box's Checked property. }
  if SortDirCb.Checked then
    StShellEnumerator1.SortDirection := sdAscending
  else
    StShellEnumerator1.SortDirection := sdDescending;
  { Refresh the list box. }
  AddItems;
end;

procedure TForm1.AddItems;
var
  I : Integer;
  S : string;
  SI : TStShellItem;
begin
  { Fill the list box with the results. }
  Screen.Cursor := crHourGlass;
  ResultsLb.Items.Clear;
  Application.ProcessMessages;
  for I := 0 to Pred(StShellEnumerator1.ShellItems.Count) do begin
    { Get the TStShellItem in the list of shell items. }
    SI := StShellEnumerator1.ShellItems[I];
    { Decide which items to add to the list box based }
    { on the selection of the Options radio group. }
    if SI.IsFileSystem then
      S := SI.Path
    else
      S := SI.DisplayName;
    if (OptionsRg.ItemIndex = 1) and (not SI.IsFolder) then
      ResultsLb.Items.AddObject(S, SI)
    else if (OptionsRg.ItemIndex = 2) and (SI.IsFolder) then
      ResultsLb.Items.AddObject(S, SI)
    else if OptionsRg.ItemIndex = 0 then
      ResultsLb.Items.AddObject(S, SI);
  end;
  Screen.Cursor := crDefault;
end;

procedure TForm1.StShellTreeView1FolderSelected(Sender: TObject;
  Folder: TStShellFolder);
begin
  if Folder.IsFileFolder and not Folder.IsDesktop then
    StShellEnumerator1.RootFolder := Folder.Path
  else if Folder.IsDesktop then
    StShellEnumerator1.SpecialRootFolder := sfDesktop
  else
    StShellEnumerator1.RootPidl := Folder.Pidl;
  { Enumerate the folder. }
  Screen.Cursor := crHourglass;
  StShellEnumerator1.Execute;
  { Add the items to the list box. }
  AddItems;
  Screen.Cursor := crDefault;
end;

end.
