object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'StShellNotification Example'
  ClientHeight = 414
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 312
    Top = 7
    Width = 153
    Height = 13
    Caption = 'Notification events for Desktop:'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 224
    Width = 297
    Height = 185
    Caption = ' Events '
    TabOrder = 0
    object AssociationCb: TCheckBox
      Left = 16
      Top = 16
      Width = 121
      Height = 17
      Caption = 'Association change'
      TabOrder = 0
      OnClick = AssociationCbClick
    end
    object AttributeCb: TCheckBox
      Left = 16
      Top = 32
      Width = 121
      Height = 17
      Caption = 'Attribute change'
      TabOrder = 1
      OnClick = AssociationCbClick
    end
    object FileCreateCb: TCheckBox
      Left = 16
      Top = 48
      Width = 121
      Height = 17
      Caption = 'File create'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = AssociationCbClick
    end
    object FileDeleteCb: TCheckBox
      Left = 16
      Top = 64
      Width = 121
      Height = 17
      Caption = 'File delete'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = AssociationCbClick
    end
    object FileRenameCb: TCheckBox
      Left = 16
      Top = 80
      Width = 121
      Height = 17
      Caption = 'File rename'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = AssociationCbClick
    end
    object FolderRenameCb: TCheckBox
      Left = 16
      Top = 144
      Width = 121
      Height = 17
      Caption = 'Folder rename'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = AssociationCbClick
    end
    object FolderChangeCb: TCheckBox
      Left = 16
      Top = 160
      Width = 121
      Height = 17
      Caption = 'Folder change'
      TabOrder = 9
      OnClick = AssociationCbClick
    end
    object FileChangeCb: TCheckBox
      Left = 16
      Top = 96
      Width = 121
      Height = 17
      Caption = 'File change'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = AssociationCbClick
    end
    object FolderCreateCb: TCheckBox
      Left = 16
      Top = 112
      Width = 121
      Height = 17
      Caption = 'Folder create'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = AssociationCbClick
    end
    object FolderDeleteCb: TCheckBox
      Left = 16
      Top = 128
      Width = 121
      Height = 17
      Caption = 'Folder delete'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = AssociationCbClick
    end
    object DriveAddCb: TCheckBox
      Left = 144
      Top = 16
      Width = 121
      Height = 17
      Caption = 'Drive add'
      TabOrder = 10
      OnClick = AssociationCbClick
    end
    object DriveRemoveCb: TCheckBox
      Left = 144
      Top = 32
      Width = 121
      Height = 17
      Caption = 'Drive remove'
      TabOrder = 11
      OnClick = AssociationCbClick
    end
    object ShellDriveAddCb: TCheckBox
      Left = 144
      Top = 48
      Width = 121
      Height = 17
      Caption = 'Drive add (shell)'
      TabOrder = 12
      OnClick = AssociationCbClick
    end
    object NetShareCb: TCheckBox
      Left = 144
      Top = 64
      Width = 121
      Height = 17
      Caption = 'Net share'
      TabOrder = 13
      OnClick = AssociationCbClick
    end
    object NetUnShareCb: TCheckBox
      Left = 144
      Top = 80
      Width = 121
      Height = 17
      Caption = 'Net unshare'
      TabOrder = 14
      OnClick = AssociationCbClick
    end
    object FreeSpaceCb: TCheckBox
      Left = 144
      Top = 144
      Width = 145
      Height = 17
      Caption = 'Folder free space change'
      TabOrder = 18
      OnClick = AssociationCbClick
    end
    object ImageListChangeCb: TCheckBox
      Left = 144
      Top = 160
      Width = 121
      Height = 17
      Caption = 'Image list change'
      TabOrder = 19
      OnClick = AssociationCbClick
    end
    object ServerDisconnectCb: TCheckBox
      Left = 144
      Top = 96
      Width = 121
      Height = 17
      Caption = 'Server disconnect'
      TabOrder = 15
      OnClick = AssociationCbClick
    end
    object MediaInsertCb: TCheckBox
      Left = 144
      Top = 112
      Width = 121
      Height = 17
      Caption = 'Media insert'
      TabOrder = 16
      OnClick = AssociationCbClick
    end
    object MediaRemoveCb: TCheckBox
      Left = 144
      Top = 128
      Width = 121
      Height = 17
      Caption = 'Media remove'
      TabOrder = 17
      OnClick = AssociationCbClick
    end
  end
  object Memo1: TMemo
    Left = 312
    Top = 21
    Width = 316
    Height = 388
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 16
    Width = 297
    Height = 201
    Caption = ' Options '
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 18
      Width = 66
      Height = 13
      Caption = 'Watch folder:'
    end
    object ActiveCb: TCheckBox
      Left = 8
      Top = 176
      Width = 97
      Height = 17
      Caption = 'Active'
      TabOrder = 0
      OnClick = ActiveCbClick
    end
    object WatchSubFoldersCb: TCheckBox
      Left = 136
      Top = 176
      Width = 105
      Height = 17
      Caption = 'Watch sub folders'
      TabOrder = 1
      OnClick = ActiveCbClick
    end
    object StShellTreeView1: TStShellTreeView
      Left = 8
      Top = 32
      Width = 280
      Height = 137
      CompressedColor = clBlue
      ExpandInterval = 2000
      Filtered = False
      Options = [toAllowRename, toAllowDrag, toAllowDrop, toExpandTopNode, toShellMenu]
      SpecialRootFolder = sfDesktop
      SpecialStartInFolder = sfNone
      OnFolderSelected = StShellTreeView1FolderSelected
      Indent = 19
      ParentColor = False
      ShowRoot = False
      TabOrder = 2
    end
  end
  object StShellNotification1: TStShellNotification
    Active = False
    NotifyEvents = [neAssociationChange, neAttributesChange, neFileChange, neFileCreate, neFileDelete, neFileRename, neDriveAdd, neDriveRemove, neShellDriveAdd, neDriveSpaceChange, neMediaInsert, neMediaRemove, neFolderCreate, neFolderDelete, neFolderRename, neFolderUpdate, neNetShare, neNetUnShare, neServerDisconnect, neImageListChange]
    SpecialWatchFolder = sfDesktop
    WatchSubFolders = False
    OnAssociationChange = StShellNotification1AssociationChange
    OnAttributeChange = StShellNotification1AttributeChange
    OnDriveAdd = StShellNotification1DriveAdd
    OnDriveRemove = StShellNotification1DriveRemove
    OnDriveSpaceChange = StShellNotification1DriveSpaceChange
    OnFileChange = StShellNotification1FileChange
    OnFileCreate = StShellNotification1FileCreate
    OnFileDelete = StShellNotification1FileDelete
    OnFileRename = StShellNotification1FileRename
    OnFolderChange = StShellNotification1FolderChange
    OnFolderCreate = StShellNotification1FolderCreate
    OnFolderDelete = StShellNotification1FolderDelete
    OnFolderRename = StShellNotification1FolderRename
    OnImageListChange = StShellNotification1ImageListChange
    OnMediaInsert = StShellNotification1MediaInsert
    OnMediaRemove = StShellNotification1MediaRemove
    OnNetShare = StShellNotification1NetShare
    OnNetUnShare = StShellNotification1NetUnShare
    OnServerDisconnect = StShellNotification1ServerDisconnect
    OnShellDriveAdd = StShellNotification1ShellDriveAdd
    Left = 264
    Top = 56
  end
end
