unit LUX.Raytrace.Material;

interface //#################################################################### ■

uses LUX, LUX.D3, LUX.Matrix.L4, LUX.Color, LUX.Color.Map.D2,
     LUX.Raytrace, LUX.Raytrace.Hit;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialRGB

     TMaterialRGB = class( TRayMaterial )
     private
     protected
     public
       ///// メソッド
       function Scatter( const WorldRay_:TSingleRay3D; const RayN_:Integer; const Hit_:TRayHit ) :TSingleRGB; override;
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
       function Scatter( const WorldRay_:TSingleRay3D; const RayN_:Integer; const Hit_:TRayHit ) :TSingleRGB; override;
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
       function Scatter( const WorldRay_:TSingleRay3D; const RayN_:Integer; const Hit_:TRayHit ) :TSingleRGB; override;
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
       function Scatter( const WorldRay_:TSingleRay3D; const RayN_:Integer; const Hit_:TRayHit ) :TSingleRGB; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialGlass

     TMaterialGlass = class( TRayMaterial )
     private
     protected
       _RefrIndex :Single;
     public
       constructor Create;
       ///// プロパティ
       property RefrIndex :Single read _RefrIndex write _RefrIndex;
       ///// メソッド
       function Scatter( const WorldRay_:TSingleRay3D; const RayN_:Integer; const Hit_:TRayHit ) :TSingleRGB; override;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.Math,
     LUX.Geometry.D3;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialRGB

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

function TMaterialRGB.Scatter( const WorldRay_:TSingleRay3D; const RayN_:Integer; const Hit_:TRayHit ) :TSingleRGB;
begin
     with TRayHitNor( Hit_ ) do
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

function TMaterialTexColor.Scatter( const WorldRay_:TSingleRay3D; const RayN_:Integer; const Hit_:TRayHit ) :TSingleRGB;
begin
     with TRayHitNorTex2D( Hit_ ) do
     begin
          Result := _Texture.Interp( Tex2D );
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

function TMaterialDiff.Scatter( const WorldRay_:TSingleRay3D; const RayN_:Integer; const Hit_:TRayHit ) :TSingleRGB;
var
   Hit :TRayHitNor;
   I :Integer;
   L :TRayLight;
   H :TRayHit;
   V :TSingle3D;
   D :Single;
begin
     Hit := TRayHitNor( Hit_ );

     Result := TSingleRGB.Create( 0, 0, 0 );

     for I := 0 to World.LightsN-1 do
     begin
          L := World.Lights[ I ];
          H := L.RayJoins( Hit_.Pos );

          if Assigned( H ) then
          begin
               V := Hit_.Pos.UnitorTo( H.Pos );

               D := DotProduct( Hit.Nor, V );  if D < 0 then D := 0;

               Result := Result + D * L.Color * _DiffRatio;

               H.Free;
          end;
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

function TMaterialMirror.Scatter( const WorldRay_:TSingleRay3D; const RayN_:Integer; const Hit_:TRayHit ) :TSingleRGB;
var
   Hit :TRayHitNor;
   RayLE :TSingleRay3D;
begin
     Hit := TRayHitNor( Hit_ );

     RayLE.Pos := Hit.Pos;
     RayLE.Vec := Reflect( WorldRay_.Vec, Hit.Nor );

     Result := World.Raytrace( RayLE, RayN_ );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterialGlass

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMaterialGlass.Create;
begin
     inherited;

     _RefrIndex := 2.417;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TMaterialGlass.Scatter( const WorldRay_:TSingleRay3D; const RayN_:Integer; const Hit_:TRayHit ) :TSingleRGB;
var
   Hit :TRayHitNor;
   ReI, ReW :Single;
   Nor :TSingle3D;
   ReA, RaA :TSingleRay3D;
   ReC, RaC :TSingleRGB;
begin
     Hit := TRayHitNor( Hit_ );

     if DotProduct( WorldRay_.Vec, Hit.Nor ) < 0 then
     begin
          ReI := _RefrIndex;
          Nor := +Hit.Nor;
     end
     else
     begin
          ReI := 1 / _RefrIndex;
          Nor := -Hit.Nor;
     end;

     ReA.Pos := Hit.Pos;
     ReA.Vec := Reflect( WorldRay_.Vec, Nor );

     ReC := World.Raytrace( ReA, RayN_ );

     if Refract( WorldRay_.Vec, Nor, ReI, RaA.Vec, ReW ) then
     begin
          RaA.Pos := Hit.Pos;

          RaC := World.Raytrace( RaA, RayN_ );

          Result := ( ReC - RaC ) * ReW + RaC;
     end
     else Result := ReC;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■