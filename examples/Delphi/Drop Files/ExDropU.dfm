object MainForm: TMainForm
  Left = 192
  Top = 107
  Caption = 'Drop Files Example Program'
  ClientHeight = 389
  ClientWidth = 525
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
  object Label1: TLabel
    Left = 192
    Top = 16
    Width = 28
    Height = 13
    Caption = 'Memo'
  end
  object Label2: TLabel
    Left = 192
    Top = 216
    Width = 37
    Height = 13
    Caption = 'List Box'
  end
  object Label3: TLabel
    Left = 8
    Top = 216
    Width = 149
    Height = 13
    Caption = 'Target String List (Memo.Lines)'
  end
  object Memo: TMemo
    Left = 192
    Top = 32
    Width = 329
    Height = 169
    TabOrder = 0
  end
  object ActiveCB: TCheckBox
    Left = 16
    Top = 16
    Width = 113
    Height = 17
    Caption = 'Drop Files Active'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = ActiveCBClick
  end
  object ListBox: TListBox
    Left = 192
    Top = 232
    Width = 329
    Height = 153
    ItemHeight = 13
    TabOrder = 2
  end
  object TargetRG: TRadioGroup
    Left = 8
    Top = 56
    Width = 161
    Height = 145
    Caption = ' Drop Target '
    ItemIndex = 0
    Items.Strings = (
      'Main Form'
      'Memo'
      'List Box')
    TabOrder = 3
    OnClick = TargetRGClick
  end
  object TargetSLMemo: TMemo
    Left = 8
    Top = 232
    Width = 169
    Height = 153
    Lines.Strings = (
      'This memo will automatically '
      'received dropped files because'
      'the memo'#39's Lines property is'
      'assigned to the StDropFiles'
      'component'#39's TargetStringList'
      'property.')
    TabOrder = 4
    WordWrap = False
  end
  object StDropFiles1: TStDropFiles
    DropTarget = Owner
    OnDropFiles = StDropFiles1DropFiles
    Left = 152
    Top = 8
  end
end
