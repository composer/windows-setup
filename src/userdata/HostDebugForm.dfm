object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Debug UserData DLL'
  ClientHeight = 261
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 288
    Height = 13
    Caption = 'This will delete your Composer cache and configuration data'
  end
  object Button1: TButton
    Left = 24
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 0
    OnClick = Button1Click
  end
  object cbSilent: TCheckBox
    Left = 24
    Top = 43
    Width = 97
    Height = 17
    Caption = 'Silent'
    TabOrder = 1
  end
  object InfoMemo: TMemo
    Left = 24
    Top = 128
    Width = 369
    Height = 120
    ScrollBars = ssBoth
    TabOrder = 2
  end
end
