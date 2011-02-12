// ***** BEGIN LICENSE BLOCK *****
// * Version: MPL 1.1
// *
// * The contents of this file are subject to the Mozilla Public License Version
// * 1.1 (the "License"); you may not use this file except in compliance with
// * the License. You may obtain a copy of the License at
// * http://www.mozilla.org/MPL/
// *
// * Software distributed under the License is distributed on an "AS IS" basis,
// * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// * for the specific language governing rights and limitations under the
// * License.
// *
// * The Original Code is TurboPower ShellShock
// *
// * The Initial Developer of the Original Code is
// * TurboPower Software
// *
// * Portions created by the Initial Developer are Copyright (C) 1996-2002
// * the Initial Developer. All Rights Reserved.
// *
// * Contributor(s):
// *
// * ***** END LICENSE BLOCK *****
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExShEnum1U.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StShlCtl"
#pragma link "SsBase"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  ResultsLb->ItemHeight = GetSystemMetrics(SM_CYSMICON);
  if (SortDirCb->Checked)
    StShellEnumerator1->SortDirection = sdAscending;
  else
    StShellEnumerator1->SortDirection = sdDescending;
  StShellEnumerator1->Sorted = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ResultsLbDrawItem(TWinControl *Control, int Index,
      TRect &Rect, TOwnerDrawState State)
{
  ResultsLb->Canvas->Pen->Color = clWindow;
  ResultsLb->Canvas->Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
  // Get the TStShellItem that corresponds to this list box item.
  TStShellItem* SI = (TStShellItem*)ResultsLb->Items->Objects[Index];
  // Get the small icon for the shell item.
  TIcon* Icon = SI->SmallIcon;
  #if (__BORLANDC__ > 0x530)
  ResultsLb->Canvas->Draw(Rect.Left, Rect.Top, Icon);
  Rect.Left += GetSystemMetrics(SM_CYSMICON) + 5;
  #else
  // Draw the icon. TIcon can only handle 32 x 32 icons in VCL
  // versions prior to 5.0 so we'll have to draw the icons the hard way.
  Graphics::TBitmap* BM1 = new Graphics::TBitmap;
  BM1->Height = Icon->Height;
  BM1->Width = Icon->Width;
  BM1->Transparent = true;
  BM1->Canvas->StretchDraw(Classes::Rect(0, 0, BM1->Width, BM1->Height), Icon);
  Graphics::TBitmap* BM2 = new Graphics::TBitmap;
  BM2->Height = GetSystemMetrics(SM_CYSMICON);
  BM2->Width = GetSystemMetrics(SM_CXSMICON);
  BM2->Transparent = true;
  BM2->Canvas->StretchDraw(Classes::Rect(0, 0, BM2->Width, BM2->Height), BM1);
  ResultsLb->Canvas->Draw(Rect.Left, Rect.Top, BM2);
  // Text will be drawn 5 pixels to the right of the icon.
  Rect.Left += BM2->Width + 5;
  delete BM1;
  delete BM2;
  #endif
  // Draw the text.
  DrawText(ResultsLb->Canvas->Handle,
    ResultsLb->Items->Strings[Index].c_str(), -1, (RECT*)&Rect,
    DT_VCENTER | DT_SINGLELINE);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SortDirCbClick(TObject *Sender)
{
  // Set the value of the SortDirection property based
  // on the value of the check box's Checked property.
  if (SortDirCb->Checked)
    StShellEnumerator1->SortDirection = sdAscending;
  else
    StShellEnumerator1->SortDirection = sdDescending;
  // Refresh the list box.
  AddItems();
}
//---------------------------------------------------------------------------
void TForm1::AddItems()
{
  // Fill the list box with the results.
  Screen->Cursor = crHourGlass;
  ResultsLb->Items->Clear();
  Application->ProcessMessages();
  for (int i=0;i<StShellEnumerator1->ShellItems->Count;i++) {
    // Get the TStShellItem in the list of shell items.
    TStShellItem* SI = StShellEnumerator1->ShellItems->Items[i];
    // Decide which items to add to the list box based
    // on the selection of the Options radio group.
    String S;
    if (SI->IsFileSystem)
      S = SI->Path;
    else
      S = SI->DisplayName;
    if ((OptionsRg->ItemIndex == 1) && (!SI->IsFolder))
      ResultsLb->Items->AddObject(S, SI);
    else if ((OptionsRg->ItemIndex == 2) && (SI->IsFolder))
      ResultsLb->Items->AddObject(S, SI);
    else if (OptionsRg->ItemIndex == 0)
      ResultsLb->Items->AddObject(S, SI);
  }
  Screen->Cursor = crDefault;
}

void __fastcall TForm1::StShellTreeView1FolderSelected(TObject *Sender,
      TStShellFolder *Folder)
{
  if (Folder->IsFileFolder && !Folder->IsDesktop)
    StShellEnumerator1->RootFolder = Folder->Path;
  else if (Folder->IsDesktop)
    StShellEnumerator1->SpecialRootFolder = sfDesktop;
  else
    StShellEnumerator1->RootPidl = Folder->Pidl;
  // Enumerate the folder.
  Screen->Cursor = crHourGlass;
  StShellEnumerator1->Execute();
  // Add the items to the list box.
  AddItems();
  Screen->Cursor = crDefault;
}
//---------------------------------------------------------------------------
