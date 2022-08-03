object FormInventoryMain: TFormInventoryMain
  Left = 0
  Top = 0
  Caption = 'FormInventoryMain'
  ClientHeight = 587
  ClientWidth = 1035
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 546
    Width = 1035
    Height = 41
    Align = alBottom
    Caption = 'Panel1'
    TabOrder = 0
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
  object SQLConnection1: TSQLConnection
    DriverName = 'Sqlite'
    Params.Strings = (
      'DriverUnit=Data.DbxSqlite'
      
        'DriverPackageLoader=TDBXSqliteDriverLoader,DBXSqliteDriver260.bp' +
        'l'
      
        'MetaDataPackageLoader=TDBXSqliteMetaDataCommandFactory,DbxSqlite' +
        'Driver260.bpl'
      'FailIfMissing=True'
      'Database=')
    Left = 512
    Top = 296
  end
  object FDConnection1: TFDConnection
    Left = 632
    Top = 280
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 496
    Top = 400
  end
end
