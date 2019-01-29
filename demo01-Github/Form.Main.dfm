object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 369
  ClientWidth = 484
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
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 185
    Height = 363
    Align = alLeft
    Caption = 'GroupBox1'
    TabOrder = 0
    ExplicitHeight = 211
    object Button1: TButton
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 175
      Height = 25
      Action = actQueryUser
      Align = alTop
      TabOrder = 0
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 5
      Top = 49
      Width = 175
      Height = 25
      Action = actQueryRepo
      Align = alTop
      TabOrder = 1
    end
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 264
    Top = 87
    Width = 185
    Height = 89
    BevelKind = bkFlat
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ActionList1: TActionList
    Left = 216
    Top = 8
    object actQueryUser: TAction
      Caption = 'actQueryUser'
      OnExecute = actQueryUserExecute
    end
    object actQueryRepo: TAction
      Caption = 'actQueryRepos'
      OnExecute = actQueryRepoExecute
    end
    object Action3: TAction
      Caption = 'Action3'
      OnExecute = Action3Execute
    end
  end
end
