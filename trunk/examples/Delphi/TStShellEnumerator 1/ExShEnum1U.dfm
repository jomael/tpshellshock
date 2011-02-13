object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'TStShellEnumerator Example'
  ClientHeight = 456
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 232
    Width = 35
    Height = 13
    Caption = 'Results'
  end
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 102
    Height = 13
    Caption = 'Folder to enumerate:'
  end
  object OptionsRg: TRadioGroup
    Left = 216
    Top = 64
    Width = 185
    Height = 89
    Caption = ' Options '
    ItemIndex = 0
    Items.Strings = (
      'Files and Items'
      'Items Only'
      'Folders Only')
    TabOrder = 0
    OnClick = SortDirCbClick
  end
  object ResultsLb: TListBox
    Left = 8
    Top = 248
    Width = 401
    Height = 201
    Style = lbOwnerDrawFixed
    TabOrder = 1
    OnDrawItem = ResultsLbDrawItem
  end
  object SortDirCb: TCheckBox
    Left = 216
    Top = 168
    Width = 97
    Height = 17
    Caption = 'Ascending Sort'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = SortDirCbClick
  end
  object StShellTreeView1: TStShellTreeView
    Left = 8
    Top = 72
    Width = 193
    Height = 153
    CompressedColor = clBlue
    ExpandInterval = 2000
    Filtered = False
    Options = [toAllowRename, toExpandTopNode, toShellMenu]
    SpecialRootFolder = sfDesktop
    SpecialStartInFolder = sfNone
    OnFolderSelected = StShellTreeView1FolderSelected
    Indent = 19
    ParentColor = False
    ShowRoot = False
    TabOrder = 3
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 393
    Height = 41
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      
        'This example shows how to enumerate a folder and display the res' +
        'ults in an '
      
        'owner-drawn list box. When you click on a folder in the tree vie' +
        'w, its items'
      'will be shown in the list view.')
    ReadOnly = True
    TabOrder = 4
  end
  object StShellEnumerator1: TStShellEnumerator
    Options = [eoIncludeFolders, eoIncludeNonFolders]
    Sorted = True
    SortDirection = sdAscending
    SpecialRootFolder = sfNone
    Left = 144
    Top = 168
  end
end
