object Form1: TForm1
  Left = 192
  Top = 106
  Caption = 'TStShortcut Example Program'
  ClientHeight = 166
  ClientWidth = 470
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
    Left = 144
    Top = 16
    Width = 90
    Height = 13
    Caption = 'Shortcut Properties'
  end
  object CreateBtn: TButton
    Left = 16
    Top = 88
    Width = 105
    Height = 25
    Caption = 'Create Shortcut'
    Enabled = False
    TabOrder = 1
    OnClick = CreateBtnClick
  end
  object Memo: TMemo
    Left = 144
    Top = 32
    Width = 321
    Height = 129
    TabOrder = 2
    WordWrap = False
  end
  object OpenBtn: TButton
    Left = 16
    Top = 48
    Width = 105
    Height = 25
    Caption = 'Select File...'
    TabOrder = 0
    OnClick = OpenBtnClick
  end
  object StShortcut1: TStShortcut
    Description = 'A description of this file'
    FileName = 'E:\Borland\Project1.exe'
    IconIndex = 0
    ShowCommand = ssMinimized
    SpecialFolder = sfDesktop
    Left = 8
    Top = 8
  end
  object OpenDialog: TOpenDialog
    Filter = 'Any File (*.*)|*.*|Shortcut Files (*.lnk)|*.lnk'
    Options = [ofHideReadOnly, ofNoDereferenceLinks]
    Left = 48
    Top = 8
  end
end
