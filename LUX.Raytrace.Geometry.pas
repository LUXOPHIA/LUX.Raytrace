unit LUX.Raytrace.Geometry;

interface //#################################################################### ■

uses LUX, LUX.D3, LUX.Graph.Tree, LUX.Raytrace;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TRayGround = class;
     TRaySphere = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayGround

     TRayGround = class( TRayGeometry )
     private
     protected
       ///// メソッド
       function _RayCast( const LocalRay_:TSingleRay3D ) :TRayHit; override;
       function HitBoundBox( const WorldRay_:TSingleRay3D ) :Boolean; override;
     public
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRaySky

     TRaySky = class( TRayGeometry )
     private
     protected
       ///// メソッド
       function _RayCast( const LocalRay_:TSingleRay3D ) :TRayHit; override;
     public
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRaySphere

     TRaySphere = class( TRayGeometry )
     private
     protected
       _Radius :Single;
       ///// アクセス
       procedure SetRadius( const Radius_:Single );
       ///// メソッド
       function _RayCast( const LocalRay_:TSingleRay3D ) :TRayHit; override;
     public
       constructor Create; override;
       ///// プロパティ
       property Radius :Single read _Radius write SetRadius;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils, System.Math;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayGround

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

function TRayGround._RayCast( const LocalRay_:TSingleRay3D ) :TRayHit;
var
   T :Single;
begin
     Result := inherited;

     if ( LocalRay_.Pos.Y > 0 ) and ( LocalRay_.Vec.Y < 0 ) then
     begin
          T := LocalRay_.Pos.Y / -LocalRay_.Vec.Y;

          if T > _EPSILON_ then
          begin
               with Result do
               begin
                    _Obj := Self;
                    _Len := T;
                    _Pos := LocalRay_.GoPos( _Len );
                    _Nor := TSingle3D.Create( 0, 1, 0 );
               end;
          end;
     end;
end;

function TRayGround.HitBoundBox( const WorldRay_:TSingleRay3D ) :Boolean;
begin
     Result := True;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRaySky

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

function TRaySky._RayCast( const LocalRay_:TSingleRay3D ) :TRayHit;
begin
     with Result do
     begin
          _Obj := Self;
          _Len := Single.MaxValue;
          _Pos := LocalRay_.GoPos( _Len );
          _Nor := -LocalRay_.Vec;

          _Tex.X := ( Pi + ArcTan2( +LocalRay_.Vec.Z, -LocalRay_.Vec.X ) ) / Pi2;
          _Tex.Y := ArcCos( LocalRay_.Vec.Y ) / Pi;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRaySphere

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

procedure TRaySphere.SetRadius( const Radius_:Single );
begin
     _Radius := Radius_;

     LocalAABB := TSingleArea3D.Create( -Radius_, +Radius_ );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

function TRaySphere._RayCast( const LocalRay_:TSingleRay3D ) :TRayHit;
var
   A, B, C, D, D2, T0, T1 :Single;
begin
     Result._Obj := nil;

     with LocalRay_ do
     begin
          A := Vec.Siz2;
          B := DotProduct( Pos, Vec );
          C := Pos.Siz2 - Pow2( _Radius );
     end;

     D := Pow2( B ) - A * C;

     if D > 0 then
     begin
          D2 := Roo2( D );

          T1 := ( -B + D2 ) / A;

          if T1 > _EPSILON_ then
          begin
               T0 := ( -B - D2 ) / A;

               with Result do
               begin
                    _Obj := Self;

                    if T0 > _EPSILON_ then _Len := T0
                                      else _Len := T1;

                    _Pos := LocalRay_.GoPos( _Len );
                    _Nor := _Pos.Unitor;
               end;
          end;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRaySphere.Create;
begin
     inherited;

     _Radius := 1;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■