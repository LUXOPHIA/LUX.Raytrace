unit LUX.Raytrace.Material;

interface //#################################################################### ■

uses LUX, LUX.D3, LUX.Matrix.L4, LUX.Color, LUX.Color.Map.D2,
     LUX.Raytrace;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialRGB

     TMaterialRGB = class( TRayMaterial )
     private
     protected
     public
       ///// メソッド
       function Scatter( const WorldEmt_:TRayHit; const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialTexColor

     TMaterialTexColor = class( TRayMaterial )
     private
       _Texture :TTexture2D;
     protected
     public
       constructor Create;
       destructor Destroy; override;
       ///// プロパティ
       property Texture :TTexture2D read _Texture;
       ///// メソッド
       function Scatter( const WorldEmt_:TRayHit; const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialDiff

     TMaterialDiff = class( TRayMaterial )
     private
     protected
       _DiffRatio :TSingleRGB;
     public
       constructor Create;
       ///// プロパティ
       property DiffRatio :TSingleRGB read _DiffRatio write _DiffRatio;
       ///// メソッド
       function Scatter( const WorldEmt_:TRayHit; const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialMirror

     TMaterialMirror = class( TRayMaterial )
     private
     protected
       _SpecRatio :TSingleRGB;
     public
       constructor Create;
       ///// プロパティ
       property SpecRatio :TSingleRGB read _SpecRatio write _SpecRatio;
       ///// メソッド
       function Scatter( const WorldEmt_:TRayHit; const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialGlass

     TMaterialGlass = class( TRayMaterial )
     private
     protected
       _RefrIndex :Single;
       _TranRatio :TSingleRGB;
     public
       constructor Create;
       ///// プロパティ
       property RefrIndex :Single     read _RefrIndex write _RefrIndex;
       property TranRatio :TSingleRGB read _TranRatio write _TranRatio;
       ///// メソッド
       function Scatter( const WorldEmt_:TRayHit; const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB; override;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils, System.Math,
     LUX.D2, LUX.Geometry.D3;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialRGB

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

function TMaterialRGB.Scatter( const WorldEmt_:TRayHit; const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB;
begin
     with WorldHit_ do
     begin
          Result.R := ( 1 + Nor.X ) / 2;
          Result.G := ( 1 + Nor.Y ) / 2;
          Result.B := ( 1 + Nor.Z ) / 2;
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialTexColor

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMaterialTexColor.Create;
begin
     inherited;

     _Texture := TTexture2D.Create;
end;

destructor TMaterialTexColor.Destroy;
begin
     _Texture.Free;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TMaterialTexColor.Scatter( const WorldEmt_:TRayHit; const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB;
begin
     with WorldHit_ do
     begin
          Result := _Texture.Interp( TSingle2D.Create( Tex.X, Tex.Y ) );
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialDiff

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMaterialDiff.Create;
begin
     inherited;

     _DiffRatio := TSingleRGB.Create( 1, 1, 1 );
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TMaterialDiff.Scatter( const WorldEmt_:TRayHit; const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB;
var
   L :TRayLight;
   A :TRayRay;
//･･････････････････････････････････････････････････････････････････････････････
     procedure Diff;
     var
        D :Single;
     begin
          D := DotProduct( WorldHit_.Nor, A.Ray.Vec );  if D < 0 then D := 0;

          Result := Result + D * L.Color * _DiffRatio;
     end;
//･･････････････････････････････････････････････････････････････････････････････
var
   I :Integer;
   H, S :TRayHit;
begin
     Result := 0;

     for I := 0 to World.LightsN-1 do
     begin
          L := World.Lights[ I ];

          L.RayJoin( WorldHit_, H );

          with A do
          begin
               Emt     := @WorldHit_;
               Ord     := WorldRay_.Ord;
               Ray.Pos := WorldHit_.Pos;
               Ray.Vec := WorldHit_.Pos.UnitorTo( H.Pos );
               Len     := 0;
               Hit     := nil;
          end;

          if World.RayCasts( WorldHit_, A, S ) then
          begin
               if S.Len >= H.Len then Diff;
          end
          else Diff;
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialMirror

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMaterialMirror.Create;
begin
     inherited;

     _SpecRatio := TSingleRGB.Create( 1, 1, 1 );
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TMaterialMirror.Scatter( const WorldEmt_:TRayHit; const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB;
var
   ReA :TRayRay;
begin
     with ReA do
     begin
          Emt     := @WorldHit_;
          Ord     := WorldRay_.Ord;
          Ray.Pos := WorldHit_.Pos;
          Ray.Vec := Reflect( WorldRay_.Ray.Vec, WorldHit_.Nor );
          Len     := 0;
          Hit     := nil;
     end;

     Result := _SpecRatio * World.Raytrace( WorldHit_, ReA );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialGlass

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMaterialGlass.Create;
begin
     inherited;

     _RefrIndex := 2.417;
     _TranRatio := TSingleRGB.Create( 1, 1, 1 );
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TMaterialGlass.Scatter( const WorldEmt_:TRayHit; const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB;
var
   ReI, ReW :Single;
   Nor :TSingle3D;
   ReA, RaA :TRayRay;
   ReC, RaC :TSingleRGB;
begin
     if DotProduct( WorldRay_.Ray.Vec, WorldHit_.Nor ) < 0 then
     begin
          ReI := _RefrIndex;
          Nor := +WorldHit_.Nor;
     end
     else
     begin
          ReI := 1 / _RefrIndex;
          Nor := -WorldHit_.Nor;
     end;

     with ReA do
     begin
          Emt     := @WorldHit_;
          Ord     := WorldRay_.Ord;
          Ray.Pos := WorldHit_.Pos;
          Ray.Vec := Reflect( WorldRay_.Ray.Vec, Nor );
          Len     := 0;
          Hit     := nil;
     end;

     ReC := World.Raytrace( WorldHit_, ReA );

     if Refract( WorldRay_.Ray.Vec, Nor, ReI, RaA.Ray.Vec, ReW ) then
     begin
          with RaA do
          begin
               Emt     := @WorldHit_;
               Ord     := WorldRay_.Ord;
               Ray.Pos := WorldHit_.Pos;
             //Ray.Vec
               Len     := 0;
               Hit     := nil;
          end;

          RaC := _TranRatio * World.Raytrace( WorldHit_, RaA );

          Result := ( ReC - RaC ) * ReW + RaC;
     end
     else Result := ReC;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■