object StOpenDialog: TStOpenDialog
  Left = 303
  Top = 263
  ActiveControl = FileNameEd
  Caption = 'Open'
  ClientHeight = 229
  ClientWidth = 663
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
  object Splitter1: TSplitter
    Left = 410
    Top = 33
    Width = 6
    Height = 133
    Align = alRight
    ExplicitLeft = 416
    ExplicitHeight = 145
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 663
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 10
      Width = 37
      Height = 13
      Caption = 'Look &in:'
    end
    object MoveUpBtn: TSpeedButton
      Left = 288
      Top = 6
      Width = 23
      Height = 22
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000012000000120000000100
        040000000000D800000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        8888880000008888888888888888880000008888888888888888880000008000
        00000000000088000000808B8B8B8B8B8B808800000080B8B8B8B8B8B8B08800
        0000808B8B0000008B808800000080B8B808B8B8B8B088000000808B8B0B8B8B
        8B808800000080B8000008B8B8B088000000808B80008B8B8B808800000080B8
        B808B8B8B8B088000000808B8B8B8B8B8B808800000080000000000000088800
        0000880B8B8B0888888888000000888000008888888888000000888888888888
        888888000000888888888888888888000000}
      OnClick = MoveUpBtnClick
    end
    object NewFolderBtn: TSpeedButton
      Left = 320
      Top = 6
      Width = 23
      Height = 22
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000012000000120000000100
        040000000000D800000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        888888000000888888888888888888000000800000000000088888000000808B
        8B8B8B8B08888800000080B8B8B8B8B8088888000000808B8B8B8B8B08888800
        000080B8B8B8B8B8088888000000808B8B8B8B8B08808800000080B8B8B8B8B8
        080888000000808B8B8B8B8B008888000000800000000000880808000000880B
        8B80888080888800000088800008880808088800000088888888808888808800
        0000888888888888088888000000888888888888888888000000888888888888
        088888000000888888888888888888000000}
      OnClick = NewFolderBtnClick
    end
    object ListBtn: TSpeedButton
      Left = 352
      Top = 6
      Width = 25
      Height = 22
      GroupIndex = 1
      Down = True
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000012000000120000000100
        040000000000D800000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        88888800000088888888888888888800000080000000000000000800000080FF
        FFFFFFFFFFFF0800000080F44FFFFF44FFFF0800000080F74F00FF84F00F0800
        000080FFFFFFFFFFFFFF0800000080F44FFFFF44FFFF0800000080F74F00FF74
        F00F0800000080FFFFFFFFFFFFFF0800000080F44FFFFF44FFFF0800000080F7
        4F00FF74F00F0800000080FFFFFFFFFFFFFF0800000080000000000000000800
        0000804444444444444408000000800000000000000008000000888888888888
        888888000000888888888888888888000000}
      OnClick = ListBtnClick
    end
    object DetailsBtn: TSpeedButton
      Left = 377
      Top = 6
      Width = 23
      Height = 22
      GroupIndex = 1
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000012000000120000000100
        040000000000D800000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        88888800000088888888888888888800000080000000000000000800000080FF
        FFFFFFFFFFFF0800000080FFFFFFFFFFFFFF0800000080FF4FF000F000FF0800
        000080FFFFFFFFFFFFFF0800000080FF4FF000F000FF0800000080FFFFFFFFFF
        FFFF0800000080FF4444444444FF0800000080FFFFFFFFFFFFFF0800000080FF
        FFF000F000FF0800000080FFFFFFFFFFFFFF0800000080000000000000000800
        0000804444444444444408000000800000000000000008000000888888888888
        888888000000888888888888888888000000}
      OnClick = DetailsBtnClick
    end
    object StShellComboBox1: TStShellComboBox
      Left = 72
      Top = 5
      Width = 209
      Height = 22
      ListView = StShellListView1
      TabOrder = 0
    end
  end
  object StShellListView1: TStShellListView
    Left = 9
    Top = 33
    Width = 401
    Height = 133
    ComboBox = StShellComboBox1
    CompressedColor = clBlue
    Filtered = False
    OpenDialogMode = True
    Optimization = otEnumerate
    Options = [loAllowRename, loShellMenu]
    RootFolder = 'c:\'
    SpecialRootFolder = sfNone
    ViewStyle = vsList
    OnItemSelected = StShellListView1ItemSelected
    Align = alClient
    MultiSelect = True
    ReadOnly = False
    TabOrder = 2
    OnDblClick = StShellListView1DblClick
  end
  object Panel2: TPanel
    Left = 0
    Top = 33
    Width = 9
    Height = 133
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 3
  end
  object Panel3: TPanel
    Left = 0
    Top = 166
    Width = 663
    Height = 63
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Label2: TLabel
      Left = 9
      Top = 12
      Width = 49
      Height = 13
      Caption = 'File &name:'
    end
    object Label3: TLabel
      Left = 9
      Top = 38
      Width = 63
      Height = 13
      Caption = 'Files of &type:'
    end
    object FileNameEd: TEdit
      Left = 80
      Top = 8
      Width = 233
      Height = 21
      TabOrder = 0
    end
    object TypeCb: TComboBox
      Left = 80
      Top = 34
      Width = 233
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = TypeCbChange
      Items.Strings = (
        'All files (*.*)'
        'Text files (*.txt)'
        'Pascal source files (*.pas)'
        'C++ source files (*.cpp;*.c;*.h;*.hpp)'
        'Image files (*.bmp;*.wmf;*.ico)')
    end
    object OpenBtn: TButton
      Left = 328
      Top = 8
      Width = 75
      Height = 23
      Caption = '&Open'
      Default = True
      TabOrder = 2
      OnClick = OpenBtnClick
    end
    object CancelBtn: TButton
      Left = 328
      Top = 37
      Width = 75
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
  end
  object Panel4: TPanel
    Left = 416
    Top = 33
    Width = 247
    Height = 133
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 4
    object Notebook1: TNotebook
      Left = 0
      Top = 0
      Width = 238
      Height = 133
      Align = alClient
      TabOrder = 0
      object TPage
        Left = 0
        Top = 0
        Caption = 'Default'
        object Label5: TLabel
          Left = 86
          Top = 65
          Width = 99
          Height = 13
          Caption = 'No preview available'
        end
      end
      object TPage
        Left = 0
        Top = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TPage
        Left = 0
        Top = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object PreviewMemo: TMemo
          Left = 0
          Top = 0
          Width = 238
          Height = 145
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 0
          WordWrap = False
        end
      end
      object TPage
        Left = 0
        Top = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object PreviewImage: TImage
          Left = 16
          Top = 0
          Width = 200
          Height = 144
          AutoSize = True
          Center = True
        end
      end
    end
    object Panel6: TPanel
      Left = 238
      Top = 0
      Width = 9
      Height = 133
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
end
