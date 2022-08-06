unit UnitIADInventoryMAIN;

interface

uses Winapi.Windows, System.Classes, Vcl.Graphics, Vcl.Forms, Vcl.Controls,
  Vcl.Menus, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.ImgList, Vcl.StdActns, Vcl.ActnList, Vcl.ToolWin, System.ImageList,
  System.Actions, Data.DB, Vcl.Grids, Vcl.DBGrids, UnitDataInterfaces,
  UnitInventory;

type
  TFormIADInventoryMain = class(TForm)
    ToolBar1: TToolBar;
    ToolButton9: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ActionList1: TActionList;
    FileNew1: TAction;
    FileOpen1: TAction;
    FileSave1: TAction;
    FileSaveAs1: TAction;
    FileExit1: TAction;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    HelpAbout1: TAction;
    StatusBar: TStatusBar;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    Edit1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    Help1: TMenuItem;
    HelpAboutItem: TMenuItem;
    DBGridStockItems: TDBGrid;
    DataSourceStockItems: TDataSource;
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure FileSave1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FConn      : IDataConnection;
    FInventory : TInventory;
  public
    { Public declarations }
  end;

var
  FormIADInventoryMain: TFormIADInventoryMain;

implementation

uses About, UnitFDMSSql, UnitFDFirebird;

{$R *.dfm}

procedure TFormIADInventoryMain.FileNew1Execute(Sender: TObject);
begin
  { Do nothing }
end;

procedure TFormIADInventoryMain.FileOpen1Execute(Sender: TObject);
begin
  { Do nothing }
end;

procedure TFormIADInventoryMain.FileSave1Execute(Sender: TObject);
begin
  { Do nothing }
end;

procedure TFormIADInventoryMain.FormCreate(Sender: TObject);
begin
  FConn := TFDInventoryMSSql.Create;
//  FConn := TFDInventoryFB.Create;
  FConn.ConnectToDB;
  FInventory := TInventory.Create(FConn);
  DataSourceStockItems.DataSet := FInventory.Query;
end;

procedure TFormIADInventoryMain.FileExit1Execute(Sender: TObject);
begin
  Close;
end;

end.
