unit LUX.Raytrace.Render;

interface //#################################################################### ■

uses FMX.Graphics,
     LUX, LUX.D2, LUX.D3, LUX.Map.D2, LUX.Color,
     LUX.Raytrace, LUX.Raytrace.Geometry, LUX.Raytrace.Material;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayRender

     TRayRender = class
     private
       _Stop :Boolean;
     protected
       _Pixels     :TBricArray2D<TSingleRGBA>;
       _World      :TRayWorld;
       _Camera     :TRayCamera;
       _MaxSampleN :Integer;
       _ConvN      :Integer;
       _ConvE      :Single;
     public
       constructor Create; overload;
       destructor Destroy; override;
       ///// プロパティ
       property Pixels     :TBricArray2D<TSingleRGBA> read _Pixels                      ;
       property World      :TRayWorld                 read _World      write _World     ;
       property Camera     :TRayCamera                read _Camera     write _Camera    ;
       property MaxSampleN :Integer                   read _MaxSampleN write _MaxSampleN;
       property ConvN      :Integer                   read _ConvN      write _ConvN     ;
       property ConvE      :Single                    read _ConvE      write _ConvE     ;
       ///// メソッド
       procedure Run;
       procedure Stop;
       procedure CopyToBitmap( const Bitmap_:TBitmap );
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.Threading, System.UITypes,
     LUX.D1;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayRender

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayRender.Create;
begin
     inherited;

     _Pixels     := TBricArray2D<TSingleRGBA>.Create( 640, 480 );
     _World      := nil;
     _Camera     := nil;
     _MaxSampleN := 64;
     _ConvN      := 4;
     _ConvE      := 1/32;
end;

destructor TRayRender.Destroy;
begin
     _Pixels.Free;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TRayRender.Run;
var
   E2 :Single;
   Pool :TThreadPool;
begin
     _Stop := False;

     E2 := Pow2( _ConvE );

     Pool := TThreadPool.Create;

     TParallel.For( 0, _Pixels.BricY-1, procedure( Y:Integer )
     var
        X :Integer;
     //･･････････････････････
          function Jitter( const Pd_:TSingle2D ) :TSingleRGB;
          var
             A :TRayRay;
          begin
               A := _Camera.Shoot( ( 0.5 + X + Pd_.X ) / _Pixels.BricX,
                                   ( 0.5 + Y + Pd_.Y ) / _Pixels.BricY );

               Result := _World.Raytrace( A );
          end;
     //･･････････････････････
     var
        N, K :Integer;
        S :TSingle2D;
        C1n, C2n, C1, C2, C1d, C2d :TSingleRGB;
     begin
          for X := 0 to _Pixels.BricX-1 do
          begin
               C1n := 0;
               C2n := 0;
               K := 0;
               for N := 1 to _MaxSampleN do
               begin
                    S := TSingle2D.RandBS4;

                    C1 := Jitter( +S );
                    C2 := Jitter( -S );

                    C1d := ( C1 - C1n ) / N;
                    C2d := ( C2 - C2n ) / N;

                    C1n := C1n + C1d;
                    C2n := C2n + C2d;

                    if Distanc2( C1n, C2n ) < E2 then
                    begin
                         Inc( K );  if K = _ConvN then Break;
                    end
                    else K := 0;
               end;

               _Pixels[ X, Y ] := Ave( C1n, C2n );
          end;
     end, Pool );

     Pool.Free;
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

     TParallel.For( 0, _Pixels.BricY-1, procedure( Y:Integer )
     var
        X :Integer;
        P :PAlphaColor;
     begin
          P := B.GetScanline( Y );
          for X := 0 to _Pixels.BricX-1 do
          begin
               P^ := _Pixels[ X, Y ];  Inc( P );
          end;
     end );

     Bitmap_.Unmap( B );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■