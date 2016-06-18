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
       ///// アクセス
       function GetLocalAABB :TSingleArea3D; override;
       ///// メソッド
       procedure _RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit; const Len_:TSingleArea ); override;
     public
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRaySky

     TRaySky = class( TRayGeometry )
     private
     protected
       ///// メソッド
       procedure _RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit; const Len_:TSingleArea ); override;
     public
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRaySphere

     TRaySphere = class( TRayGeometry )
     private
     protected
       _Radius :Single;
       ///// アクセス
       function GetLocalAABB :TSingleArea3D; override;
       ///// メソッド
       procedure _RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit; const Len_:TSingleArea ); override;
     public
       constructor Create; override;
       ///// プロパティ
       property Radius :Single read _Radius write _Radius;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayImplicit

     TRayImplicit = class( TRayGeometry )
     private
     protected
       _IteraN :Integer;
       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; virtual; abstract;
       procedure _RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit; const Len_:TSingleArea ); override;
     public
       constructor Create; override;
       ///// プロパティ
       property IteraN :Integer read _IteraN write _IteraN;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayTorus

     TRayTorus = class( TRayImplicit )
     private
     protected
       _LingR :Single;
       _PipeR :Single;
       ///// アクセス
       function GetLocalAABB :TSingleArea3D; override;
       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
       constructor Create; override;
       ///// プロパティ
       property LingR :Single read _LingR write _LingR;
       property PipeR :Single read _PipeR write _PipeR;
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

/////////////////////////////////////////////////////////////////////// アクセス

function TRayGround.GetLocalAABB :TSingleArea3D;
begin
     Result := TSingleArea3D.PoMax;

     with Result do
     begin
          Min.Y := 0;
          Max.Y := 0;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

procedure TRayGround._RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit; const Len_:TSingleArea );
var
   T :Single;
begin
     if ( LocalRay_.Ray.Pos.Y > 0 ) and ( LocalRay_.Ray.Vec.Y < 0 ) then
     begin
          T := LocalRay_.Ray.Pos.Y / -LocalRay_.Ray.Vec.Y;

          if T > 0 then
          begin
               with LocalRay_ do
               begin
                    Len := T;
               end;

               with LocalHit_ do
               begin
                    Obj := Self;

                    Nor := TSingle3D.Create( 0, 1, 0 );
               end;
          end;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRaySky

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

procedure TRaySky._RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit; const Len_:TSingleArea );
begin
     with LocalRay_ do
     begin
          Len := Single.MaxValue;
     end;

     with LocalHit_ do
     begin
          Obj := Self;

          Nor := -LocalRay_.Ray.Vec;

          Tex.X := ( Pi + ArcTan2( +LocalRay_.Ray.Vec.Z, -LocalRay_.Ray.Vec.X ) ) / Pi2;
          Tex.Y := ArcCos( LocalRay_.Ray.Vec.Y ) / Pi;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRaySphere

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TRaySphere.GetLocalAABB :TSingleArea3D;
begin
     Result := TSingleArea3D.Create( -_Radius, +_Radius );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

procedure TRaySphere._RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit; const Len_:TSingleArea );
var
   B, C, D, D2, T0, T1 :Single;
begin
     with LocalRay_.Ray do
     begin
          B := DotProduct( Pos, Vec );
          C := Pos.Siz2 - Pow2( _Radius );
     end;

     D := Pow2( B ) - C;

     if D > 0 then
     begin
          D2 := Roo2( D );

          T1 := -B + D2;

          if T1 > 0 then
          begin
               T0 := -B - D2;

               with LocalRay_ do
               begin
                    if T0 > 0 then Len := T0
                              else Len := T1;
               end;

               with LocalHit_ do
               begin
                    Obj := Self;

                    Nor := LocalHit_.Pos.Unitor;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayImplicit

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

procedure TRayImplicit._RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit; const Len_:TSingleArea );
var
   P0, P1 :TSingle3D;
   D0, D1,
   T0, T1 :Single;
   S :TValueSign;
   N :Integer;
begin
     with LocalRay_ do
     begin
          if Len_.Min > 0 then Len := Len_.Min;

          P0 := Tip;

          D0 := DistanceFunc( P0 ).o;  S := Sign( D0 );

          T0 := S * D0;

          Len := Len + T0;
     end;

     for N := 1 to _IteraN do
     begin
          with LocalRay_ do
          begin
               P1 := Tip;

               D1 := DistanceFunc( P1 ).o;

               T1 := S * D1;

               Len := Len + T1;

               if Len > Len_.Max then Exit;
          end;

          if ( T1 < T0 ) and ( T1 < _EPSILON_ ) then
          begin
               with LocalHit_ do
               begin
                    Obj := Self;

                    Nor := Nabla( DistanceFunc, P1 ).Unitor;
               end;

               with LocalRay_ do
               begin
                    Len := Len - D1 / DotProduct( LocalHit_.Nor, Ray.Vec );
               end;

               Exit;
          end;

          T0 := T1;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayImplicit.Create;
begin
     inherited;

     _IteraN := 1024;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayTorus

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TRayTorus.GetLocalAABB :TSingleArea3D;
var
   R :Single;
begin
     R := _LingR + _PipeR;

     Result := TSingleArea3D.Create( -R, -R, -_PipeR,
                                     +R, +R, +_PipeR );
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