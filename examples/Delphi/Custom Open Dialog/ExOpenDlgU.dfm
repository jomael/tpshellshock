object Form1: TForm1
  Left = 193
  Top = 109
  Caption = 'Custom Open Dialog Example'
  ClientHeight = 226
  ClientWidth = 360
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
    Left = 104
    Top = 24
    Width = 94
    Height = 13
    Caption = 'Selected file: (none)'
  end
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = '&Open'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 16
    Top = 56
    Width = 321
    Height = 145
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'This example uses a TStShellComboBox and TStShellListView to '
      'create a custom file open dialog. It does not use the '
      'TStDialogPanel (the prefered method).')
    TabOrder = 1
  end
end
