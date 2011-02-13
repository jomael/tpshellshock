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

unit ExShEnum2U;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StShlCtl, StdCtrls, ComCtrls, SsBase;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    StShellEnumerator1: TStShellEnumerator;
    StShellTreeView1: TStShellTreeView;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure StShellTreeView1FolderSelected(Sender: TObject;
      Folder: TStShellFolder);
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
var
  I : Integer;
begin
  Memo1.Lines.Clear;
  StShellEnumerator1.Execute;
  Memo1.Lines.Add('FOLDERS');
  for I := 0 to Pred(StShellEnumerator1.ShellItems.Count) do
    if StShellEnumerator1.ShellItems[I].IsFolder then begin
      if StShellEnumerator1.ShellItems[I].IsFileSystem then
        Memo1.Lines.Add(StShellEnumerator1.ShellItems[I].Path)
      else
        Memo1.Lines.Add(StShellEnumerator1.ShellItems[I].DisplayName)
    end;
  Memo1.Lines.Add('');
  Memo1.Lines.Add('ITEMS');
  for I := 0 to Pred(StShellEnumerator1.ShellItems.Count) do
    if not StShellEnumerator1.ShellItems[I].IsFolder then
      Memo1.Lines.Add(StShellEnumerator1.ShellItems[I].Path);
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
end;

end.
