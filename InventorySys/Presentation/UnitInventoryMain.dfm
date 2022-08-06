object FormInventoryMain: TFormInventoryMain
  Left = 0
  Top = 0
  Caption = 'FormInventoryMain'
  ClientHeight = 587
  ClientWidth = 847
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object PanelButtons: TPanel
    Left = 0
    Top = 546
    Width = 847
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitWidth = 1035
    object ButtonAddProduct: TButton
      Left = 152
      Top = 8
      Width = 129
      Height = 25
      Caption = 'Add Product'
      TabOrder = 0
      OnClick = ButtonAddProductClick
    end
  end
  object DBGridStockItems: TDBGrid
    Left = 0
    Top = 0
    Width = 847
    Height = 546
    Align = alClient
    DataSource = DataSourceStockItems
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
  end
  object DataSourceStockItems: TDataSource
    Left = 760
    Top = 416
  end
end
