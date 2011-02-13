object Form1: TForm1
  Left = 196
  Top = 121
  Caption = 'TStShellEnumerator Example'
  ClientHeight = 313
  ClientWidth = 609
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 208
    Top = 8
    Width = 39
    Height = 13
    Caption = 'Results:'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 102
    Height = 13
    Caption = 'Folder to enumerate:'
  end
  object Button1: TButton
    Left = 47
    Top = 279
    Width = 99
    Height = 25
    Caption = '&Enumerate Folder'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 208
    Top = 24
    Width = 393
    Height = 281
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object StShellTreeView1: TStShellTreeView
    Left = 8
    Top = 24
    Width = 193
    Height = 241
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
    TabOrder = 2
  end
  object StShellEnumerator1: TStShellEnumerator
    Options = [eoIncludeFolders, eoIncludeNonFolders]
    Sorted = True
    SortDirection = sdAscending
    SpecialRootFolder = sfDesktop
    Left = 88
    Top = 144
  end
end
