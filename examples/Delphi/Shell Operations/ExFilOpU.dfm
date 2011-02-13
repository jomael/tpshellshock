object Form1: TForm1
  Left = 200
  Top = 108
  Caption = 'Shell Operations Test Program'
  ClientHeight = 213
  ClientWidth = 476
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
    Left = 16
    Top = 16
    Width = 37
    Height = 13
    Caption = 'Source:'
  end
  object Label2: TLabel
    Left = 256
    Top = 16
    Width = 58
    Height = 13
    Caption = 'Destination:'
  end
  object DstEdit: TEdit
    Left = 256
    Top = 32
    Width = 193
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 208
    Top = 32
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 448
    Top = 32
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 399
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Go...'
    TabOrder = 3
    OnClick = Button3Click
  end
  object FileOpGroup: TRadioGroup
    Left = 256
    Top = 72
    Width = 129
    Height = 137
    Caption = ' File Operation '
    ItemIndex = 0
    Items.Strings = (
      'Copy'
      'Delete'
      'Move'
      'Rename')
    TabOrder = 4
  end
  object SourceMemo: TMemo
    Left = 16
    Top = 32
    Width = 193
    Height = 177
    TabOrder = 5
    WordWrap = False
  end
  object StBrowser: TStBrowser
    SpecialRootFolder = sfDrives
    SpecialRootFolderID = 17
    Left = 136
    Top = 1
  end
  object StFileOperation1: TStFileOperation
    Destination = 'e:\test'
    Options = [foAllowUndo, foFilesOnly, foNoConfirmMkDir, foRenameCollision]
    OnError = StFileOperation1Error
    Left = 96
    Top = 1
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect]
    Left = 176
    Top = 1
  end
end
