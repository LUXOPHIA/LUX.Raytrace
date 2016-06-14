unit LUX.Raytrace.Geometry;

interface //#################################################################### ■

uses LUX, LUX.D1, LUX.D2, LUX.D3, LUX.Graph.Tree, LUX.Raytrace;

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
       function _RayCast( const LocalEmt_:TRayHit; const LocalRay_:TRayRay; var LocalHit_:TRayHit ) :Boolean; override;
     public
       ///// メソッド
       function HitBoundBox( const WorldRay_:TRayRay; out MinT_,MaxT_:Single ) :Boolean; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRaySky

     TRaySky = class( TRayGeometry )
     private
     protected
       ///// メソッド
       function _RayCast( const LocalEmt_:TRayHit; const LocalRay_:TRayRay; var LocalHit_:TRayHit ) :Boolean; override;
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
       function _RayCast( const LocalEmt_:TRayHit; const LocalRay_:TRayRay; var LocalHit_:TRayHit ) :Boolean; override;
     public
       constructor Create; override;
       ///// プロパティ
       property Radius :Single read _Radius write SetRadius;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayImplicit

     TRayImplicit = class( TRayGeometry )
     private
     protected
       _IteraN :Integer;
       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; virtual; abstract;
       function _RayCast( const LocalEmt_:TRayHit; const LocalRay_:TRayRay; var LocalHit_:TRayHit ) :Boolean; override;
     public
       constructor Create; override;
       ///// プロパティ
       property IteraN :Integer read _IteraN write _IteraN;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayTorus

     TRayTorus = class( TRayImplicit )
     private
       ///// メソッド
       procedure MakeAABB;
     protected
       _LingR :Single;
       _PipeR :Single;
       ///// アクセス
       procedure SetLingR( const LingR_:Single );
       procedure SetPipeR( const PipeR_:Single );
       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
       constructor Create; override;
       ///// プロパティ
       property LingR :Single read _LingR write SetLingR;
       property PipeR :Single read _PipeR write SetPipeR;
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

function TRayGround._RayCast( const LocalEmt_:TRayHit; const LocalRay_:TRayRay; var LocalHit_:TRayHit ) :Boolean;
var
   T :Single;
begin
     if ( LocalRay_.Ray.Pos.Y > 0 ) and ( LocalRay_.Ray.Vec.Y < 0 ) then
     begin
          T := LocalRay_.Ray.Pos.Y / -LocalRay_.Ray.Vec.Y;

          if T > _EPSILON_ then
          begin
               with LocalHit_ do
               begin
                    Obj := Self;
                    Len := T;
                    Pos := LocalRay_.Ray.GoPos( Len );
                    Nor := TSingle3D.Create( 0, 1, 0 );
               end;

               Result := True;
          end
          else Result := False;
     end
     else Result := False;
end;

function TRayGround.HitBoundBox( const WorldRay_:TRayRay; out MinT_,MaxT_:Single ) :Boolean;
begin
     Result := True;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRaySky

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

function TRaySky._RayCast( const LocalEmt_:TRayHit; const LocalRay_:TRayRay; var LocalHit_:TRayHit ) :Boolean;
begin
     with LocalHit_ do
     begin
          Obj := Self;
          Len := Single.MaxValue;
          Pos := LocalRay_.Ray.GoPos( Len );
          Nor := -LocalRay_.Ray.Vec;

          Tex.X := ( Pi + ArcTan2( +LocalRay_.Ray.Vec.Z, -LocalRay_.Ray.Vec.X ) ) / Pi2;
          Tex.Y := ArcCos( LocalRay_.Ray.Vec.Y ) / Pi;
     end;

     Result := True;
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

function TRaySphere._RayCast( const LocalEmt_:TRayHit; const LocalRay_:TRayRay; var LocalHit_:TRayHit ) :Boolean;
var
   A, B, C, D, D2, T0, T1 :Single;
begin
     Result := False;

     with LocalRay_.Ray do
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

               with LocalHit_ do
               begin
                    Obj := Self;

                    if T0 > _EPSILON_ then Len := T0
                                      else Len := T1;

                    Pos := LocalRay_.Ray.GoPos( Len );
                    Nor := Pos.Unitor;
               end;

               Result := True;
          end;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRaySphere.Create;
begin
     inherited;

     _Radius := 1;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayImplicit

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

function TRayImplicit._RayCast( const LocalEmt_:TRayHit; const LocalRay_:TRayRay; var LocalHit_:TRayHit ) :Boolean;
var
   L, T, D0, D1 :Single;
   V :TSingle3D;
   N, S :Integer;
   AP, P :TdSingle3D;
begin
     L := LocalRay_.Ray.Vec.Size;
     V := LocalRay_.Ray.Vec;

     S := Sign( DotProduct( LocalEmt_.Nor, V ) );

     AP := LocalRay_.Ray.Pos + S * LocalEmt_.Nor * _EPSILON_;

     D0 := S * DistanceFunc( AP ).o;

     T := D0;

     for N := 1 to _IteraN do
     begin
          P := AP + V * T;

          D1 := S * DistanceFunc( P ).o;

          T := T + D1;

          if ( D1 < D0 ) and ( D1 < _EPSILON_ ) and ( _EPSILON_ < T ) then
          begin
               with LocalHit_ do
               begin
                    Obj := Self;
                    Len := T;
                    Nor := Nabla( DistanceFunc, P ).Unitor;
                    Pos := P - D1 * S * Nor;
               end;

               Result := True;

               Exit;
          end;

          D0 := D1;
     end;

     Result := False;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayImplicit.Create;
begin
     inherited;

     _IteraN := 100;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayTorus

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// メソッド

procedure TRayTorus.MakeAABB;
var
   R :Single;
begin
     R := _LingR + _PipeR;

     LocalAABB := TSingleArea3D.Create( -R, -R, -_PipeR,
                                        +R, +R, +_PipeR );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

procedure TRayTorus.SetLingR( const LingR_:Single );
begin
     _LingR := LingR_;

     MakeAABB;
end;

procedure TRayTorus.SetPipeR( const PipeR_:Single );
begin
     _PipeR := PipeR_;

     MakeAABB;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TRayTorus.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
var
   A, B :TdSingle2D;
begin
     A.X := P_.X;
     A.Y := P_.Y;

     B.X := A.Size - _LingR;
     B.Y := P_.Z;

     Result := B.Size - _PipeR;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayTorus.Create;
begin
     inherited;

     _LingR := 2;
     _PipeR := 1;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■