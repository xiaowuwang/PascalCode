unit UnitSimpleBank;

{$APPTYPE CONSOLE}

interface

uses Classes, SysUtils;

type

  IAccount = interface
    procedure Deposit(anAmount : Currency);
    procedure WithDraw(anAmount : Currency);
    function PrintStatement : Integer;
    function GetAccountName : String;
    function GetAccountBalance : Currency;
  end;

  TAccount = class(TInterfacedObject, IAccount)
  private
    FName    : String;
    FBalance : Currency;
    FTransaction : TStringList;
  public
    constructor Create(aName : String; anAmount : Currency);
    procedure Deposit(anAmount : Currency);
    procedure WithDraw(anAmount : Currency);
    function GetAccountName : String;
    function GetAccountBalance : Currency;
    function PrintStatement : Integer;
    property Name : String read FName;
    property Balance : Currency read FBalance;
  end;

  TSimpleBank = class
  private
    FAccount : IAccount;
    Function GetAccountName : String;
    Function GetBalance: Currency;
  public
    constructor CreateAccount(anAccount : IAccount);
    procedure Deposit(anAmount : Currency);
    procedure WithDraw(anAmount : Currency);
    function PrintMiniStatement : Integer;
    property AccountName : String read GetAccountName;
    property Balance : Currency read GetBalance;
  end;

implementation

//Class TSimpleBank implementation start here
constructor TSimpleBank.CreateAccount(anAccount : IAccount);
begin
  inherited Create;
  FAccount := anAccount;
end;

Function TSimpleBank.GetAccountName : String;
begin
  result := FAccount.GetAccountName;
end;

Function TSimpleBank.GetBalance: Currency;
begin
  result := FAccount.GetAccountBalance;
end;

procedure TSimpleBank.Deposit(anAmount : Currency);
begin
  FAccount.Deposit(anAmount);
end;

procedure TSimpleBank.WithDraw(anAmount : Currency);
begin
  FAccount.WithDraw(anAmount);
end;

function TSimpleBank.PrintMiniStatement : Integer;
begin
  result := FAccount.PrintStatement;
end;

//Class TAccount implementation start here
constructor TAccount.Create(aName : String; anAmount : Currency);
begin
  inherited Create;
  FTransaction := TStringList.Create;
  FName    := aName;
  FBalance := anAmount;
  FTransaction.Add(FormatDateTime('dd/mm/yy hh:nn:ss ',Now)+aName + ' Open an account, deposit '+FloatToStrF(anAmount, ffCurrency,4,2));
end;

procedure TAccount.Deposit(anAmount : Currency);
begin
  if (anAmount > 0) then
  begin
    FBalance := FBalance + anAmount;
    FTransaction.Add(FormatDateTime('dd/mm/yy hh:nn:ss : ',Now)+FName + ' Deposit ' + FloatToStrF(anAmount, ffCurrency,4,2));
  end
  else
    WriteLn('nedative deposit not allowed');
end;

procedure TAccount.WithDraw(anAmount : Currency);
begin
  if (anAmount <= FBalance) then
  begin
    FBalance := FBalance - anAmount;
    FTransaction.Add(FormatDateTime('dd/mm/yy hh:nn:ss : ',Now)+FName + ' WithDraw ' + FloatToStrF(anAmount, ffCurrency,4,2));
  end
  else
    WriteLn('overdrawing not allowed');
end;

function TAccount.PrintStatement : Integer;
var
  i : Integer;
begin
  FTransaction.Add(FormatDateTime('dd/mm/yy hh:nn:ss : ',Now)+FName + ' Balance ' + FloatToStrF(FBalance, ffCurrency,4,2));
  for I := 0 to FTransaction.Count-1 do
    WriteLn(FTransaction[i]);
  result := FTransaction.Count;
  FTransaction.SaveToFile(FName+'MiniStatement.txt');
  WriteLn('The mini statement:'+FName+'MiniStatement.txt'+' printed in the folder.');
  Readln;
end;

function TAccount.GetAccountName : String;
begin
  result := FName;
end;

function TAccount.GetAccountBalance : Currency;
begin
  result := FBalance;
end;

end.
