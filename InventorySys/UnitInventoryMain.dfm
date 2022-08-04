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
  object FDConnection1: TFDConnection
    Params.Strings = (
      'DriverID=MSSQL'
      'Server=STEVENLAPTOP'
      ''
      'User_Name=stevenwang'
      'Password=W5Passw0rd')
    Connected = True
    LoginPrompt = False
    Left = 192
    Top = 176
  end
  object FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    Left = 360
    Top = 176
  end
  object FDScript1: TFDScript
    SQLScripts = <>
    Connection = FDConnection1
    Params = <>
    Macros = <>
    Left = 512
    Top = 296
  end
end
