unit Com_DBHelper;

interface

uses Windows, Sysutils, Classes, DB;

type

  TDataSet_Helper = class helper for TDataset
  public
    procedure OpenOrRefresh;
  end;

implementation

uses StrUtils;

{ TDataSetHelper }

procedure TDataSet_Helper.OpenOrRefresh;
begin
  if not Active then
    Open
  else
    begin
      try
        Refresh;
      except
        Close;
        Open;
      end;
    end;
end;

end.