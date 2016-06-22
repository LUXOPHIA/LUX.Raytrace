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
       _RecLipC :Single;
       ///// アクセス
       function GetLipC :Single;
       procedure SetLipC( const LipC_:Single );
     protected
       _IteraN :Integer;
       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; virtual; abstract;
       procedure _RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit; const Len_:TSingleArea ); override;
       ///// プロパティ
       property LipC :Single read GetLipC write SetLipC;
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

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDeformImp

     TDeformImp = class( TRayImplicit )
     private
     protected
       _ImpObj :TRayImplicit;
     public
       constructor Create; override;
       ///// プロパティ
       property ImpObj :TRayImplicit read _ImpObj write _ImpObj ;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDefTwist

     TDefTwist = class( TDeformImp )
     private
       _Scale :Single;
       ///// メソッド
       procedure Update;
     protected
       _Angle :Single;
       ///// アクセス
       function GetLocalAABB :TSingleArea3D; override;
       procedure SetAngle( const Angle_:Single );
       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
       constructor Create; override;
       ///// プロパティ
       property Angle :Single read _Angle write SetAngle;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDefTaper

     TDefTaper = class( TDeformImp )
     private
       ///// メソッド
       procedure Update;
     protected
       _BotY :Single; 
       _TopY :Single;
       _BotS :Single;
       _TopS :Single;
       ///// アクセス
       function GetLocalAABB :TSingleArea3D; override;
       procedure SetBotY( const BotY_:Single );
       procedure SetTopY( const TopY_:Single );
       procedure SetBotS( const BotS_:Single );
       procedure SetTopS( const TopS_:Single );
       ///// メソッド
       function Deform( const Y_:TdSingle ) :TdSingle;
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
       constructor Create; override;
       ///// プロパティ
       property BotY :Single read _BotY write SetBotY;
       property TopY :Single read _TopY write SetTopY;
       property BotS :Single read _BotS write SetBotS;
       property TopS :Single read _TopS write SetTopS;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayWave

     TRayWave = class( TRayImplicit )
     private
       _Const :Single;
       ///// メソッド
       procedure MakeFreqs;
       procedure CalcLipC;
     protected
       _Freqs  :TArray2<TSingle3D>;
       _FreqsN :Integer;
       ///// アクセス
       function GetLocalAABB :TSingleArea3D; override;
       procedure SetFreqsN( const FreqsN_:Integer );
       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
       constructor Create; override;
       ///// プロパティ
       property FreqsN :Integer read _FreqsN write SetFreqsN;
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

/////////////////////////////////////////////////////////////////////// アクセス

function TRayImplicit.GetLipC :Single;
begin
     Result := 1 / _RecLipC;
end;

procedure TRayImplicit.SetLipC( const LipC_:Single );
begin
     _RecLipC := 1 / LipC_;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

procedure TRayImplicit._RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit; const Len_:TSingleArea );
var
   MinL, C,
   D0, D1 :Single;
   P0, P1 :TSingle3D;
   N :Integer;
begin
     if Len_.Min > 0 then MinL := Len_.Min
                     else MinL := 0;

     with LocalRay_ do
     begin
          Len := MinL;

          P0 := Tip;

          D0 := DistanceFunc( P0 ).o;  C := _RecLipC * Sign( D0 );

          Len := Len + C * D0;

          for N := 2 to _IteraN do
          begin
               if ( Len < MinL ) or ( Len_.Max < Len ) then Exit;

               P1 := Tip;

               D1 := DistanceFunc( P1 ).o;

               Len := Len + C * D1;

               if ( Abs( D1 ) < Abs( D0 ) ) and ( Abs( D1 ) < _EPSILON_ ) then
               begin
                    with LocalHit_ do
                    begin
                         Obj := Self;
                         Nor := Nabla( DistanceFunc, P1 ).Unitor;
                    end;

                    Len := Len - D1 / DotProduct( LocalHit_.Nor, Ray.Vec );

                    Exit;
               end;

               D0 := D1;
          end;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayImplicit.Create;
begin
     inherited;

     LipC := 1;

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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDeformImp

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDeformImp.Create;
begin
     inherited;

    _ImpObj := nil;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDefTwist

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDefTwist.Update;
begin
     _Scale := _Angle / 4;

     LipC := Roo2( 4 + Pow2( Pi / _Scale ) );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDefTwist.GetLocalAABB :TSingleArea3D;
var
   RanY :TSIngleArea;
   MaxR, R :Single;
   I :Integer;
begin
     with _ImpObj.LocalAABB do
     begin
          RanY := TSingleArea.NeInf;
          MaxR := 0;
          for I := 0 to 7 do
          begin
               with Poin[ I ] do
               begin
                    with RanY do
                    begin
                         if Y < Min then Min := Y;
                         if Max < Y then Max := Y;
                    end;

                    R := Roo2( Pow2( X ) + Pow2( Z ) );

                    if MaxR < R then MaxR := R;
               end;
          end;
     end;

     Result := TSingleArea3D.Create( -MaxR, RanY.Min, -MaxR,
                                     +MaxR, RanY.Max, +MaxR );
end;

procedure TDefTwist.SetAngle( const Angle_:Single );
begin
     _Angle := Angle_;  Update;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TDefTwist.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
var
   T :TdSingle;
   P :TdSingle3D;
begin
     T := _Scale * P_.Y;

     P.X := P_.X * Cos( T ) - P_.Z * Sin( T );
     P.Y := P_.Y;
     P.Z := P_.X * Sin( T ) + P_.Z * Cos( T );

     Result := _ImpObj.DistanceFunc( P );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDefTwist.Create;
begin
     inherited;

     _Angle := DegToRad( 90 );

     Update;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDefTaper

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// メソッド

procedure TDefTaper.Update;
begin
     LipC := Max( Deform( _BotY ).o, Deform( _TopY ).o );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TDefTaper.GetLocalAABB :TSingleArea3D;
var
   I :Integer;
   S :Single;
   P :TSingle3D;
begin
     Result := TSingleArea3D.NeInf;

     with _ImpObj.LocalAABB do
     begin
          Result.Min.Y := Min.Y;
          Result.Max.Y := Max.Y;

          for I := 0 to 7 do
          begin
               with Poin[ I ] do
               begin
                    S := Deform( Y ).o;

                    P.X := S * X;
                    P.Y :=     Y;
                    P.Z := S * Z;
               end;

               with Result, P do
               begin
                    if X < Min.X then Min.X := X;
                    if Z < Min.Z then Min.Z := Z;

                    if Max.X < X then Max.X := X;
                    if Max.Z < Z then Max.Z := Z;
               end;
          end;
     end;
end;

//------------------------------------------------------------------------------

procedure TDefTaper.SetBotY( const BotY_:Single );
begin
     _BotY := BotY_;  Update;
end;

procedure TDefTaper.SetTopY( const TopY_:Single );
begin
     _TopY := TopY_;  Update;
end;

procedure TDefTaper.SetBotS( const BotS_:Single );
begin
     _BotS := BotS_;  Update;
end;

procedure TDefTaper.SetTopS( const TopS_:Single );
begin
     _TopS := TopS_;  Update;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TDefTaper.Deform( const Y_:TdSingle ) :TdSingle;
var
   T :TdSingle;
begin
     T := ( Y_ - _BotY ) / ( _TopY - _BotY );

     Result := 1 / ( ( _TopS - _BotS ) * T + _BotS );
end;

function TDefTaper.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
var
   S :TdSingle;
   P :TdSingle3D;
begin
     S := Deform( P_.Y );

     P.X := S * P_.X;
     P.Y :=     P_.Y;
     P.Z := S * P_.Z;

     Result := _ImpObj.DistanceFunc( P );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDefTaper.Create;
begin
     inherited;

     _BotY := -4;
     _TopY := +4;
     _BotS := 1.9;
     _TopS := 0.1;

     Update;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayWave

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// メソッド

procedure TRayWave.MakeFreqs;
var
   X, Y :Integer;
begin
     SetLength( _Freqs, _FreqsN, _FreqsN );

     for Y := 0 to _FreqsN-1 do
     begin
          for X := 0 to _FreqsN-1 do
          begin
               with _Freqs[ X, Y ] do
               begin
                    X := Pi2 * Random;
                    Y := Pi2 * Random;

                    Z := Random / ( ( X + 1 ) * ( Y + 1 ) );
               end;
          end;
     end;

     _Freqs[ 0, 0 ].Z := 0;
end;

procedure TRayWave.CalcLipC;
var
   D :Single;
   X, Y :Integer;
begin
     D := 0;
     for Y := 0 to _FreqsN-1 do
     begin
          for X := 0 to _FreqsN-1 do D := D + _Freqs[ X, Y ].Z * ( X + Y );
     end;

     _Const := Roo2( Pow2( Pi * D ) + 1 );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TRayWave.GetLocalAABB :TSingleArea3D;
var
   H :Single;
   X, Y :Integer;
begin
     H := 0;
     for Y := 0 to _FreqsN-1 do
     begin
          for X := 0 to _FreqsN-1 do H := H + _Freqs[ X, Y ].Z;
     end;

     Result := TSingleArea3D.Create( -1, -H, -1,
                                     +1, +H, +1 );
end;

procedure TRayWave.SetFreqsN( const FreqsN_:Integer );
begin
     _FreqsN := FreqsN_;

     MakeFreqs;
     CalcLipC;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TRayWave.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
var
   H :TdSingle;
   X, Y :Integer;
   F :TSingle3D;
begin
     H := 0;
     for Y := 0 to _FreqsN-1 do
     begin
          for X := 0 to _FreqsN-1 do
          begin
               F := _Freqs[ X, Y ];

               H  := H  + F.Z * Sin( F.X + X * Pi * P_.X )
                              * Sin( F.Y + Y * Pi * P_.Z );
          end;
     end;

     Result := ( P_.Y - H ) / _Const;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayWave.Create;
begin
     inherited;

     _FreqsN := 4;

     MakeFreqs;
     CalcLipC;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■