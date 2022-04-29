program SimpleBankServer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  UnitSimpleBank in 'UnitSimpleBank.pas';

var
 aSimpleBank : TSimpleBank;
 anAccount   : IAccount;

begin
  Writeln('Welcome to Simple Bank');
  aSimpleBank := TSimpleBank.Create;
  try
    anAccount := TAccount.Create('Steven',1000);
    aSimpleBank.CreateAccount(anAccount);
    aSimpleBank.Deposit(268);
    aSimpleBank.Deposit(380);
    aSimpleBank.WithDraw(525);
    aSimpleBank.PrintMiniStatement;
  finally
    aSimpleBank.Free;
  end;
end.
