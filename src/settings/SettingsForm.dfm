object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Composer - Shell Menu Settings'
  ClientHeight = 435
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnButtons: TPanel
    Left = 8
    Top = 404
    Width = 410
    Height = 23
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 1
    object btnClose: TButton
      Left = 335
      Top = 0
      Width = 75
      Height = 23
      Caption = 'Close'
      TabOrder = 0
      OnClick = btnCloseClick
    end
  end
  object pcSettings: TPageControl
    Left = 8
    Top = 8
    Width = 410
    Height = 390
    ActivePage = tsMenus
    TabOrder = 0
    object tsMenus: TTabSheet
      Caption = 'Shell Menus'
      OnShow = tsMenusShow
      object pnMenus: TPanel
        Left = 10
        Top = 15
        Width = 380
        Height = 340
        BevelOuter = bvNone
        TabOrder = 0
        object lbMenus: TLabel
          Left = 0
          Top = 0
          Width = 380
          Height = 26
          AutoSize = False
          Caption = 
            'Run Composer from Windows Explorer by right-clicking folder item' +
            's, using either the Command Prompt or a console of your choice. ' +
            ' '
          WordWrap = True
        end
        object gbDefault: TGroupBox
          Left = 0
          Top = 215
          Width = 380
          Height = 60
          Caption = 'Default Console'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          TabOrder = 0
          object lbDefault: TLabel
            Left = 10
            Top = 27
            Width = 360
            Height = 13
            AutoSize = False
            Caption = 'Command Prompt'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
        end
        object gbStatus: TGroupBox
          Left = 0
          Top = 45
          Width = 380
          Height = 60
          Caption = 'Status'
          TabOrder = 1
          object pnStatus: TPanel
            Left = 10
            Top = 22
            Width = 360
            Height = 23
            BevelEdges = []
            BevelOuter = bvNone
            Locked = True
            TabOrder = 0
            object lbStatus: TLabel
              Left = 0
              Top = 5
              Width = 190
              Height = 13
              AutoSize = False
              Caption = 'Shell Menus are not installed'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object btnStatus: TButton
              Left = 200
              Top = 0
              Width = 75
              Height = 23
              Caption = 'Add'
              DoubleBuffered = False
              ParentDoubleBuffered = False
              TabOrder = 0
              OnClick = btnStatusClick
            end
          end
        end
        object gbCollapse: TGroupBox
          Left = 0
          Top = 130
          Width = 380
          Height = 60
          Caption = 'Collapse Menu'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          TabOrder = 2
          object cbCollapse: TCheckBox
            Left = 10
            Top = 27
            Width = 360
            Height = 17
            Caption = 'Select this option to show the menu items from a single entry'
            TabOrder = 0
            OnClick = cbCollapseClick
          end
        end
        object lblHelp: TLinkLabel
          Left = 0
          Top = 320
          Width = 197
          Height = 17
          Caption = 'For more details see <a href="url">Shell Menu Settings</a>'
          Color = clWindow
          ParentColor = False
          TabOrder = 3
          OnLinkClick = lblHelpLinkClick
        end
      end
    end
    object tsConsole: TTabSheet
      Caption = 'Console'
      ImageIndex = 1
      OnShow = tsConsoleShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnConsoles: TPanel
        Left = 10
        Top = 15
        Width = 380
        Height = 240
        BevelOuter = bvNone
        TabOrder = 0
        object edProgram: TEdit
          Tag = 185
          Left = 0
          Top = 210
          Width = 380
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clInactiveCaptionText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 4
        end
        object lvShells: TListView
          Left = 0
          Top = 45
          Width = 260
          Height = 125
          Columns = <
            item
              Caption = 'Name'
              Width = 155
            end
            item
              AutoSize = True
              Caption = 'Default'
            end>
          ColumnClick = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          ParentFont = False
          ShowColumnHeaders = False
          TabOrder = 1
          ViewStyle = vsReport
          OnChange = lvShellsChange
          OnCustomDrawItem = lvShellsCustomDrawItem
          OnCustomDrawSubItem = lvShellsCustomDrawSubItem
          OnMouseUp = lvShellsMouseUp
        end
        object lblIntro: TLinkLabel
          Left = 0
          Top = 0
          Width = 380
          Height = 35
          AutoSize = False
          Caption = 
            'Set the Default Console to use. For more details, including how ' +
            'to use a different console see <a href="url">Shell Menu Settings' +
            '</a>.'
          Color = clWindow
          ParentColor = False
          TabOrder = 0
          TabStop = True
          OnLinkClick = lblIntroLinkClick
        end
        object pnShellbtns: TPanel
          Left = 270
          Top = 45
          Width = 75
          Height = 125
          BevelOuter = bvNone
          TabOrder = 2
          object btnAdd: TButton
            Left = 0
            Top = 27
            Width = 75
            Height = 23
            Caption = 'Add'
            DoubleBuffered = False
            ParentDoubleBuffered = False
            TabOrder = 1
            OnClick = btnAddClick
          end
          object btnReset: TButton
            Left = 0
            Top = 102
            Width = 75
            Height = 23
            Caption = 'Reset'
            DoubleBuffered = False
            Enabled = False
            ParentDoubleBuffered = False
            TabOrder = 0
            OnClick = btnResetClick
          end
          object btnDefault: TButton
            Left = 0
            Top = 0
            Width = 75
            Height = 23
            Caption = 'Set Default'
            DoubleBuffered = False
            ParentDoubleBuffered = False
            TabOrder = 2
            OnClick = btnDefaultClick
          end
          object btnSave: TButton
            Left = 0
            Top = 75
            Width = 75
            Height = 23
            Caption = 'Save'
            TabOrder = 3
            OnClick = btnSaveClick
          end
        end
        object lblProperties: TLinkLabel
          Left = 0
          Top = 180
          Width = 82
          Height = 17
          Caption = '<a href="">Show properties</a>'
          Color = clWindow
          ParentColor = False
          TabOrder = 3
          TabStop = True
          OnClick = lblPropertiesClick
        end
      end
      object gbParameters: TGroupBox
        Left = 10
        Top = 260
        Width = 380
        Height = 85
        Caption = 'Parameters'
        TabOrder = 1
        object pnRun: TPanel
          Left = 10
          Top = 50
          Width = 360
          Height = 21
          BevelEdges = []
          BevelOuter = bvNone
          Locked = True
          TabOrder = 1
          object lbRun: TLabel
            Left = 0
            Top = 3
            Width = 23
            Height = 13
            Caption = 'Run:'
          end
          object btnRun: TButton
            Left = 320
            Top = 0
            Width = 40
            Height = 21
            Caption = 'Run'
            DoubleBuffered = False
            ParentDoubleBuffered = False
            TabOrder = 1
            OnClick = btnRunClick
          end
          object edRun: TEdit
            Left = 35
            Top = 0
            Width = 280
            Height = 21
            TabOrder = 0
            OnChange = edRunChange
          end
        end
        object pnOpen: TPanel
          Left = 10
          Top = 22
          Width = 360
          Height = 21
          BevelEdges = []
          BevelOuter = bvNone
          Locked = True
          TabOrder = 0
          object lbOpen: TLabel
            Left = 0
            Top = 3
            Width = 30
            Height = 13
            Caption = 'Open:'
          end
          object btnOpen: TButton
            Left = 320
            Top = 0
            Width = 40
            Height = 21
            Caption = 'Open'
            DoubleBuffered = False
            ParentDoubleBuffered = False
            TabOrder = 1
            OnClick = btnOpenClick
          end
          object edOpen: TEdit
            Left = 35
            Top = 0
            Width = 280
            Height = 21
            TabOrder = 0
            OnChange = edOpenChange
          end
        end
      end
    end
  end
end
