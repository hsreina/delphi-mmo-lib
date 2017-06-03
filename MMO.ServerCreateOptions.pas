unit MMO.ServerCreateOptions;

interface

type
  TServerCreateOptions = record
    ThreadSafe: Boolean;
    Port: UInt16;
    MaxPlayers: UInt16;
  end;

implementation

end.
