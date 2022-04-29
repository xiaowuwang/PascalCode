unit UnitSimpleBankServerTests;

interface

uses
  DUnitX.TestFramework, UnitSimpleBank;

type
  [TestFixture]
  TSimpleBankServerTests = class
  public
    [Test]
    procedure TestCreateAccount;
    [Test]
    [TestCase('Test Normal Deposit','500,600')]
    [TestCase('Test Negative Deposit','500,-600')]
    procedure TestDeposit(const InitDeposit : Currency; const  DepositAmount: Currency);
    [Test]
    [TestCase('TestNormalDrawing','500,300')]
    [TestCase('TestOverDrawing','500,600')]
    procedure TestWithDraw(const InitDeposit : Currency; const  WithDrawAmount: Currency);
    [Test]
    procedure TestMiniStatement;
  end;

implementation
uses
  SysUtils;

procedure TSimpleBankServerTests.TestCreateAccount;
var
  Expected : String;
  Actual   : String;
  aSimpleBank : TSimpleBank;
  anAccount   : IAccount;
begin
  //Arrange
  Expected := 'Steven'+FloatToStrF(100, ffCurrency,4,2);
  aSimpleBank := TSimpleBank.Create;
  try
    //Act
    anAccount := TAccount.Create('Steven',100);
    aSimpleBank.CreateAccount(anAccount);
    actual   := aSimpleBank.AccountName+ FloatToStrF(aSimpleBank.Balance, ffCurrency,4,2);
    //Assert
    Assert.AreEqual(Expected, actual, 'Simple Bank Create Account unsuccessful' );
  finally
    aSimpleBank.Free;
  end;
end;

procedure TSimpleBankServerTests.TestDeposit(const InitDeposit : Currency; const  DepositAmount: Currency);
var
  Expected: Currency;
  Actual: Currency;
  aSimpleBank : TSimpleBank;
  anAccount   : IAccount;
begin
  aSimpleBank := TSimpleBank.Create;
  try
    anAccount := TAccount.Create('Steven',InitDeposit);
    aSimpleBank.CreateAccount(anAccount);
    //Arrange
    Expected := InitDeposit + DepositAmount;
    //Act
    aSimpleBank.Deposit(DepositAmount);
    Actual := aSimpleBank.Balance;
    //Assert
    Assert.AreEqual(Expected, Actual, 'aSimpleBank Deposit unsuccessful');
  finally
    aSimpleBank.Free;
  end;
end;

procedure TSimpleBankServerTests.TestWithDraw(const InitDeposit : Currency; const  WithDrawAmount: Currency);
var
  Expected: Currency;
  Actual: Currency;
  aSimpleBank : TSimpleBank;
  anAccount   : IAccount;
begin
  aSimpleBank := TSimpleBank.Create;
  try
    anAccount := TAccount.Create('Steven',InitDeposit);
    aSimpleBank.CreateAccount(anAccount);
    //Arrange
    Expected := InitDeposit - WithDrawAmount;
    //Act
    aSimpleBank.WithDraw(WithDrawAmount);
    Actual := aSimpleBank.Balance;
    //Assert
    Assert.AreEqual(Expected, Actual, 'aSimpleBank WithDraw unsuccessful');
  finally
    aSimpleBank.Free;
  end;
end;

procedure TSimpleBankServerTests.TestMiniStatement;
var
  Expected: Integer;
  Actual: Integer;
  aSimpleBank : TSimpleBank;
  anAccount   : IAccount;
begin
  //Arrange
  Expected := 5;
  aSimpleBank := TSimpleBank.Create;
  try
    //Act
    anAccount := TAccount.Create('Steven',1000);
    aSimpleBank.CreateAccount(anAccount);
    aSimpleBank.Deposit(268);
    aSimpleBank.Deposit(380);
    aSimpleBank.WithDraw(525);
    Actual := aSimpleBank.PrintMiniStatement;
    //Assert
    Assert.AreEqual(Expected, Actual, 'aSimpleBank MiniStatement unsuccessful');
  finally
    aSimpleBank.Free;
  end;

end;

initialization
  TDUnitX.RegisterTestFixture(TSimpleBankServerTests);

end.
