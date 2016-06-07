unit LUX.Raytrace.Render;

interface //#################################################################### ■

uses FMX.Graphics,
     LUX, LUX.D3, LUX.Map.D2, LUX.Color,
     LUX.Raytrace, LUX.Raytrace.Hit, LUX.Raytrace.Geometry, LUX.Raytrace.Material;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayRender

     TRayRender = class
     private
       _Stop :Boolean;
     protected
       _Pixels  :TBricArray2D<TSingleRGBA>;
       _World   :TRayWorld;
       _Camera  :TRayCamera;
       _SampleN :Integer;
       ///// アクセス
     public
       constructor Create; overload;
       destructor Destroy; override;
       ///// プロパティ
       property Pixels  :TBricArray2D<TSingleRGBA> read _Pixels                ;
       property World   :TRayWorld                 read _World   write _World  ;
       property Camera  :TRayCamera                read _Camera  write _Camera ;
       property SampleN :Integer                   read _SampleN write _SampleN;
       ///// メソッド
       procedure Run;
       procedure Stop;
       procedure CopyToBitmap( const Bitmap_:TBitmap );
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.Threading,
     LUX.D1;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayRender

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayRender.Create;
begin
     inherited;

     _Pixels  := TBricArray2D<TSingleRGBA>.Create( 640, 480 );
     _World   := nil;
     _Camera  := nil;
     _SampleN := 1;
end;

destructor TRayRender.Destroy;
begin
     _Pixels.Free;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TRayRender.Run;
begin
     _Stop := False;

     TParallel.For( 0, _Pixels.BricY-1,
     procedure( Y:Integer )
     var
        X, N :Integer;
        C :TSingleRGBA;
        A :TSingleRay3D;
     begin
          for X := 0 to _Pixels.BricX-1 do
          begin
               C := 0;
               for N := 1 to _SampleN do
               begin
                    A := _Camera.Shoot( ( 0.5 + X + TSingle.RandomBS4 ) / _Pixels.BricX,
                                        ( 0.5 + Y + TSingle.RandomBS4 ) / _Pixels.BricY );

                    C := C + _World.Raytrace( A, 1 );
               end;
               C := C / _SampleN;
               C.A := 1;

               _Pixels[ X, Y ] := C;
          end;
     end );
end;

procedure TRayRender.Stop;
begin
     _Stop := True;
end;

procedure TRayRender.CopyToBitmap( const Bitmap_:TBitmap );
var
   B :TBitmapData;
begin
     Bitmap_.SetSize( _Pixels.BricX, _Pixels.BricY );

     Bitmap_.Map( TMapAccess.Write, B );

     TParallel.For( 0, _Pixels.BricY-1,
     procedure( Y:Integer )
     var
        X :Integer;
     begin
          for X := 0 to _Pixels.BricX-1 do
          begin
               B.Color[ X, Y ] := _Pixels[ X, Y ];
          end;
     end );

     Bitmap_.Unmap( B );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■