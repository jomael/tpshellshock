object MainForm: TMainForm
  Left = 201
  Top = 110
  Caption = 'Shell Browser Example Program'
  ClientHeight = 380
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object AboutBtn: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'About...'
    TabOrder = 0
    OnClick = AboutBtnClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 48
    Width = 553
    Height = 329
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 216
      Width = 68
      Height = 13
      Caption = 'Display Name:'
    end
    object DisplayNameLabel: TLabel
      Left = 96
      Top = 216
      Width = 209
      Height = 13
      AutoSize = False
    end
    object Label2: TLabel
      Left = 56
      Top = 243
      Width = 29
      Height = 13
      Caption = 'Path: '
    end
    object PathLabel: TLabel
      Left = 96
      Top = 243
      Width = 209
      Height = 13
      AutoSize = False
    end
    object Label3: TLabel
      Left = 23
      Top = 269
      Width = 65
      Height = 13
      Caption = 'Image Index:'
    end
    object ImageIndexLabel: TLabel
      Left = 96
      Top = 269
      Width = 49
      Height = 13
      AutoSize = False
    end
    object Label4: TLabel
      Left = 8
      Top = 296
      Width = 78
      Height = 13
      Caption = 'Selected Folder:'
    end
    object SelectedFolderLabel: TLabel
      Left = 96
      Top = 296
      Width = 409
      Height = 13
      AutoSize = False
    end
    object Label5: TLabel
      Left = 27
      Top = 120
      Width = 60
      Height = 13
      Caption = 'Root Folder:'
    end
    object Label6: TLabel
      Left = 8
      Top = 168
      Width = 78
      Height = 13
      Caption = 'Selected Folder:'
    end
    object Label7: TLabel
      Left = 24
      Top = 72
      Width = 61
      Height = 13
      Caption = 'Start Folder:'
    end
    object BrowseBtn: TButton
      Left = 16
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Browse...'
      TabOrder = 0
      OnClick = BrowseBtnClick
    end
    object GroupBox2: TGroupBox
      Left = 312
      Top = 24
      Width = 217
      Height = 233
      Caption = 'Browse Options'
      TabOrder = 1
      object ComputerCB: TCheckBox
        Left = 16
        Top = 24
        Width = 185
        Height = 17
        Caption = 'Browse For Computer'
        TabOrder = 0
      end
      object PrinterCB: TCheckBox
        Left = 16
        Top = 53
        Width = 185
        Height = 17
        Caption = 'Browse For Printer'
        TabOrder = 1
      end
      object DomainCB: TCheckBox
        Left = 16
        Top = 82
        Width = 185
        Height = 17
        Caption = 'Don'#39't Go Below Domain'
        TabOrder = 2
      end
      object AncestorCB: TCheckBox
        Left = 16
        Top = 110
        Width = 185
        Height = 17
        Caption = 'Return File System Ancestors'
        TabOrder = 3
      end
      object DirectoriesCB: TCheckBox
        Left = 16
        Top = 139
        Width = 185
        Height = 17
        Caption = 'Return File System Directories'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object ShowFilesCB: TCheckBox
        Left = 16
        Top = 168
        Width = 97
        Height = 17
        Caption = 'Show Files'
        TabOrder = 5
      end
      object ShowEditCB: TCheckBox
        Left = 16
        Top = 200
        Width = 137
        Height = 17
        Caption = 'Show Edit Control'
        TabOrder = 6
      end
    end
    object RootEdit: TEdit
      Left = 96
      Top = 112
      Width = 185
      Height = 21
      TabOrder = 3
    end
    object SelFolderEdit: TEdit
      Left = 96
      Top = 160
      Width = 185
      Height = 21
      TabOrder = 4
    end
    object StartFolderCombo: TComboBox
      Left = 96
      Top = 64
      Width = 185
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'AltStartup'
        'AppData'
        'BitBucket'
        'CommonAltStartup'
        'CommonDesktopDir'
        'CommonFavorites'
        'CommonPrograms'
        'CommonStartMenu'
        'CommonStartup'
        'Controls'
        'Cookies'
        'Desktop'
        'DesktopDir'
        'Drives'
        'Favorites'
        'Fonts'
        'History'
        'Internet'
        'InternetCache'
        'NetHood'
        'Network'
        '(None)'
        'Personal'
        'Printers'
        'PrintHood'
        'Programs'
        'RecentFiles'
        'SendTo'
        'StartMenu'
        'Startup'
        'Templates')
    end
  end
  object StShellAbout1: TStShellAbout
    AdditionalText = 'Copyright '#169' 1998, TurboPower Software Co.'
    Caption = 'Shell Browser Example'
    Icon.Data = {
      0000010001002020100000000000E80200001600000028000000200000004000
      0000010004000000000080020000000000000000000000000000000000000000
      000000008000008000000080800080000000800080008080000080808000C0C0
      C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF009999
      99999999999999999999999999999FFFFFFFFFFFFF88888888888FFFFFF997FF
      FFFFFFFF888888888888888FFFF9977FFFFFFFF8444C444888888888FFF99777
      FFFFFF4444444444488888888FF997777FFF44444C4C4C4C4C48888888F99777
      77F44444443444444444888888F99777774C444C433C4C4C4C4C488888899777
      74444444C334C444C444C48888899777744C4C4C433C4C4C4C4C4C8888899777
      444444C43334CCC4C4C4C44888899777444C4C43333C4C4C4C4C4C3888899774
      4444C4433333CCCCC4CCC433888997744C4C4C4333333C4C4C4C4C3388899774
      4444C43333333CCCCCCCC43388899774444C4C333333CC4CCC4C4C3388899774
      4444C433333CCCCCCCC33333888997744C4C4C334C4C4CCCCCC3333388899774
      44444433CCCC3CCCCCC3333388F99777444C4C433C433C4CCC4C333888F99777
      4444343333333CCCCCCCC4C88FF99777744C333333333C4C4C433C88FFF99777
      7444333333333CCCCCC3348FFFF99777774C333333333C4C3C433FFFFFF99777
      7774333333C333CC3433FFFFFFF9977777774333334C333C4C377FFFFFF99777
      77777744433444C4477777FFFFF99777777777774C4C4C477777777FFFF99777
      777777777777777777777777FFF997777777777777777777777777777FF99777
      77777777777777777777777777F9999999999999999999999999999999990000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000}
    Left = 112
    Top = 8
  end
  object StBrowser1: TStBrowser
    AdditionalText = 'Additional Text'
    Caption = 'SysTools Browse for Folder'
    Options = []
    SpecialRootFolder = sfNone
    OnSelChanged = StBrowser1SelChanged
    Left = 128
    Top = 72
  end
end
