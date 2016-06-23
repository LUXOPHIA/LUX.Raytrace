unit LUX.Raytrace.Geometry;

interface //#################################################################### ■

uses LUX, LUX.D1, LUX.D2, LUX.D3, LUX.Map.D2, LUX.Graph.Tree, LUX.Raytrace;

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

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayField

     TRayField = class( TRayGeometry )
     private
       ///// メソッド
       procedure MakeWave( const FreqsN_:Integer );
     protected
       _Grids :TGridArray2D<Single>;
       ///// アクセス
       function GetLocalAABB :TSingleArea3D; override;
       ///// メソッド
       function GridPoin( const X_,Y_:Integer ) :TSingle3D;
       function HitSurface( const X_,Y_:Integer; var Ray_:TRayRay; var Hit_:TRayHit; const L0_,L1_:Single ) :Boolean;
       procedure _RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit; const Len_:TSingleArea ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Grids :TGridArray2D<Single> read _Grids             ;
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayField

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

procedure TRayField.MakeWave( const FreqsN_:Integer );
var
   FX, FY, X, Y :Integer;
   F :TSingle3D;
   SX, SY :Single;
begin
     for FY := 0 to FreqsN_ do
     for FX := 0 to FreqsN_ do
     begin
          if ( FX = 0 ) and ( FY = 0 ) then
          begin
               for Y := -1 to _Grids.GridY do
               begin
                    for X := -1 to _Grids.GridX do _Grids[ X, Y ] := X / _Grids.GridX;
               end;
          end
          else
          begin
               F.X := Pi2 * Random;
               F.Y := Pi2 * Random;
               F.Z := 2 * Random / ( ( FX + 1 ) * ( FY + 1 ) );

               for Y := -1 to _Grids.GridY do
               begin
                    SY := Sin( F.Y + Pi * FY * Y / _Grids.GridY-1 );

                    for X := -1 to _Grids.GridX do
                    begin
                         SX := Sin( F.X + Pi * FX * X / _Grids.GridX-1 );

                         _Grids[ X, Y ] := _Grids[ X, Y ] + F.Z * SX * SY;
                    end;
               end;
          end;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TRayField.GetLocalAABB :TSingleArea3D;
var
   SZ :TSingleArea;
   X, Y :Integer;
   Z :Single;
begin
     with _Grids do
     begin
          SZ := TSingleArea.NeMax;
          for Y := 0 to GridX-1 do
          begin
               for X := 0 to GridX-1 do
               begin
                    Z := Grid[ X, Y ];

                    if Z < SZ.Min then SZ.Min := Z;
                    if SZ.Max < Z then SZ.Max := Z;
               end;
          end;

          Result := TSingleArea3D.Create(  0   ,  0   , SZ.Min,
                                          BricX, BricY, SZ.Max );
     end;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TRayField.GridPoin( const X_,Y_:Integer ) :TSingle3D;
begin
     with Result do
     begin
          X := X_;
          Y := Y_;
          Z := _Grids[ X_, Y_ ];
     end;
end;

function TRayField.HitSurface( const X_,Y_:Integer; var Ray_:TRayRay; var Hit_:TRayHit; const L0_,L1_:Single ) :Boolean;
var
   G00, G01, G10, G11, N0, N1, C0, C1 :TSingle3D;
   D0, D1 :Single;
begin
     G10 := GridPoin( X_+0, Y_+1 );  G11 := GridPoin( X_+1, Y_+1 );
     G00 := GridPoin( X_+0, Y_+0 );  G01 := GridPoin( X_+1, Y_+0 );

     N0 := CrossProduct( G00.VectorTo( G01 ), G00.VectorTo( G10 ) ).Unitor;
     N1 := CrossProduct( G11.VectorTo( G10 ), G11.VectorTo( G01 ) ).Unitor;

     D0 := DotProduct( N0, G00 );
     D1 := DotProduct( N1, G11 );

     Result := False;

     Ray_.Len := ( D0 - DotProduct( Ray_.Ray.Pos, N0 ) ) / DotProduct( Ray_.Ray.Vec, N0 );

     if ( Ray_.Len < L0_ ) or ( L1_ < Ray_.Len ) then Exit;

     C0 := Ray_.Tip;

     if ( X_+0 <= C0.X ) and ( C0.X <= X_+1 ) and
        ( Y_+0 <= C0.Y ) and ( C0.Y <= Y_+1 ) and
        ( C0.X + C0.Y <= X_ + Y_ + 1 ) then
     begin
          Result := True;

          Hit_.Nor := N0;

          Exit;
     end;

     Ray_.Len := ( D1 - DotProduct( Ray_.Ray.Pos, N1 ) ) / DotProduct( Ray_.Ray.Vec, N1 );

     if ( Ray_.Len < L0_ ) or ( L1_ < Ray_.Len ) then Exit;

     C1 := Ray_.Tip;

     if ( X_+0 <= C1.X ) and ( C1.X <= X_+1 ) and
        ( Y_+0 <= C1.Y ) and ( C1.Y <= Y_+1 ) and
        ( X_ + Y_ + 1 <= C1.X + C1.Y ) then
     begin
          Result := True;

          Hit_.Nor := N1;

          Exit;
     end;
end;

procedure TRayField._RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit; const Len_:TSingleArea );
var
   P, P0, P1, G00, G01, G10, G11 :TSingle3D;
   T, dT :TSingle2D;
   I0, I1, dI :TInteger2D;
   L0, L1, Z :Single;
begin
     L0 := Max( 0, Len_.Min ) + 0.01;

     with LocalRay_.Ray do
     begin
          dI.X := Sign( Vec.X );
          dI.Y := Sign( Vec.Y );

          dT.X := dI.X / Vec.X;
          dT.Y := dI.Y / Vec.Y;

          P := GoPos( L0 );

          if dI.X < 0 then
          begin
               I0.X := Ceil( P.X ) - 1;

               T .X := L0 + ( I0.X   - P.X ) / Vec.X;
          end
          else
          begin
               I0.X := Floor( P.X );

               T .X := L0 + ( I0.X+1 - P.X ) / Vec.X;
          end;

          if dI.Y < 0 then
          begin
               I0.Y := Ceil( P.Y ) - 1;

               T .Y := L0 + ( I0.Y   - P.Y ) / Vec.Y;
          end
          else
          begin
               I0.Y := Floor( P.Y );

               T .Y := L0 + ( I0.Y+1 - P.Y ) / Vec.Y;
          end;
     end;

     while ( 0 <= I0.X ) and ( I0.X < _Grids.BricX )
       and ( 0 <= I0.Y ) and ( I0.Y < _Grids.BricY )
       and ( L0 < Len_.Max ) do
     begin
          if T.X < T.Y then
          begin
               L1 := T.X;

               T .X := T .X + dT.X;

               I1.X := I0.X + dI.X;
               I1.Y := I0.Y       ;
          end
          else
          if T.X > T.Y then
          begin
               L1 := T.Y;

               T .Y := T .Y + dT.Y;

               I1.X := I0.X       ;
               I1.Y := I0.Y + dI.Y;
          end
          else Exit;

          if HitSurface( I0.X, I0.Y, LocalRay_, LocalHit_, L0, L1 ) then
          begin
               LocalHit_.Obj := Self;

               Exit;
          end;

          L0 := L1;
          I0 := I1;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayField.Create;
begin
     inherited;

     _Grids := TGridArray2D<Single>.Create( 100, 100, 1, 1 );

     MakeWave( 4 );
end;

destructor TRayField.Destroy;
begin
     _Grids.Free;
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

               H := H + F.Z * Sin( F.X + X * Pi * P_.X )
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