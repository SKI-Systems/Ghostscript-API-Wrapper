object FMain: TFMain
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Ghostscript API Example'
  ClientHeight = 620
  ClientWidth = 760
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 760
    Height = 620
    ActivePage = Tab_Operation
    Align = alClient
    TabOrder = 0
    object Tab_Operation: TTabSheet
      Caption = 'Convert'
      DesignSize = (
        752
        592)
      object SBtn_OpenFile: TSpeedButton
        Left = 719
        Top = 9
        Width = 23
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '...'
        OnClick = SBtn_OpenFileClick
      end
      object BBtn_Test: TBitBtn
        Left = 8
        Top = 42
        Width = 75
        Height = 25
        Caption = 'Convert'
        TabOrder = 0
        OnClick = BBtn_TestClick
      end
      object LEd_PdfFile: TLabeledEdit
        Left = 32
        Top = 10
        Width = 681
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 20
        EditLabel.Height = 13
        EditLabel.Caption = 'File:'
        LabelPosition = lpLeft
        TabOrder = 1
      end
      object M_UserParams: TMemo
        Left = 8
        Top = 83
        Width = 744
        Height = 129
        Hint = 'You can insert here Ghostscrip Userparams'
        Anchors = [akLeft, akTop, akRight]
        ParentShowHint = False
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 2
      end
      object M_Output: TMemo
        Left = 8
        Top = 216
        Width = 744
        Height = 248
        Hint = 'Shows the Ghostscript Standard output'
        Anchors = [akLeft, akTop, akRight, akBottom]
        ParentShowHint = False
        ReadOnly = True
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 3
      end
      object M_Errors: TMemo
        Left = 8
        Top = 470
        Width = 744
        Height = 122
        Hint = 'Shows the Errors'
        Anchors = [akLeft, akRight, akBottom]
        ParentShowHint = False
        ReadOnly = True
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 4
      end
      object RGrp_Devices: TRadioGroup
        Left = 89
        Top = 34
        Width = 624
        Height = 40
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Convert Device'
        Columns = 5
        ItemIndex = 0
        Items.Strings = (
          'Display PDF'
          'Display PDF-A'
          'PDF'
          'PDF-A')
        TabOrder = 5
      end
    end
    object Tab_PDFView: TTabSheet
      Caption = 'PDF Preview'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object P_PDF_Top: TPanel
        Left = 0
        Top = 0
        Width = 752
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object UpDown_Pages: TUpDown
          Left = 239
          Top = 6
          Width = 17
          Height = 24
          TabOrder = 0
          OnClick = UpDown_PagesClick
        end
        object LEd_PageCount: TLabeledEdit
          AlignWithMargins = True
          Left = 202
          Top = 12
          Width = 30
          Height = 12
          Alignment = taRightJustify
          BevelEdges = []
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          EditLabel.Width = 37
          EditLabel.Height = 13
          EditLabel.BiDiMode = bdLeftToRight
          EditLabel.Caption = 'Seiten: '
          EditLabel.ParentBiDiMode = False
          LabelPosition = lpLeft
          ReadOnly = True
          TabOrder = 1
          Text = '0/0'
        end
      end
      object ScrollBoxImage: TScrollBox
        Left = 0
        Top = 41
        Width = 752
        Height = 551
        Align = alClient
        TabOrder = 1
        object Img_Page: TImage
          Left = 0
          Top = 0
          Width = 741
          Height = 452
          AutoSize = True
        end
      end
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'PDF-Datei (*.pdf)|*.pdf|Alle Dateien (*.*)|*.*'
    Left = 424
    Top = 128
  end
end
