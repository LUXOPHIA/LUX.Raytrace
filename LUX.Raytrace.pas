unit LUX.Raytrace;

interface //#################################################################### ■

uses LUX, LUX.D1, LUX.D3, LUX.Matrix.L4, LUX.Color, LUX.Graph.Tree;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     PRayRay = ^TRayRay;
     PRayHit = ^TRayHit;

     TRayGeometry = class;
       TRayCamera = class;
       TRayLight  = class;
       TRayWorld  = class;
     TRayMaterial = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayRay

     TRayRay = record
     private
       ///// アクセス
       function GetTip :TSingle3D;
       procedure SetTip( const Tip_:TSingle3D );
     public
       Emt :PRayHit;
       Ord :Integer;
       Ray :TSingleRay3D;
       Len :Single;
       Hit :PRayHit;
       ///// プロパティ
       property Tip :TSingle3D read GetTip write SetTip;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayHit

     TRayHit = record
     private
       ///// アクセス
       function GetPos :TSingle3D;
     public
       Ray :PRayRay;
       Obj :TRayGeometry;
       Nor :TSingle3D;
       Tan :TSingle3D;
       Bin :TSingle3D;
       Tex :TSingle3D;
       ///// プロパティ
       property Pos :TSingle3D read GetPos;
       ///// メソッド
       function Scatter( const WorldRay_:TRayRay ) :TSingleRGB;
     end;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayGeometry

     TRayGeometry = class( TTreeNode<TRayGeometry> )
     private
       ///// メソッド
       ///procedure UpFlagMatrix;
     protected
       _LocalMatrix :TSingleM4    ;  up_LocalMatrix:Boolean;
       _LocalMatriI :TSingleM4    ;  up_LocalMatriI:Boolean;
       _WorldMatrix :TSingleM4    ;  up_WorldMatrix:Boolean;
       _WorldMatriI :TSingleM4    ;  up_WorldMatriI:Boolean;
       _WorldAABB   :TSingleArea3D;  up_WorldAABB:Boolean;
       _Material    :TRayMaterial ;
       ///// アクセス
       function GetWorld :TRayWorld; virtual;
       function GetLocalMatrix :TSingleM4; virtual;
       procedure SetLocalMatrix( const LocalMatrix_:TSingleM4 ); virtual;
       function GetLocalMatriI :TSingleM4; virtual;
       procedure SetLocalMatriI( const LocalMatriI_:TSingleM4 ); virtual;
       function GetWorldMatrix :TSingleM4; virtual;
       procedure SetWorldMatrix( const WorldMatrix_:TSingleM4 ); virtual;
       function GetWorldMatriI :TSingleM4; virtual;
       procedure SetWorldMatriI( const WorldMatriI_:TSingleM4 ); virtual;
       function GetLocalAABB :TSingleArea3D; virtual;
       function GetWorldAABB :TSingleArea3D; virtual;
       function GetMaterial :TRayMaterial; virtual;
       procedure SetMaterial( const Material_:TRayMaterial ); virtual;
       ///// メソッド
       procedure _RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit ); virtual;
       procedure _RayJoin( var LocalRay_:TRayRay; var LocalHit_:TRayHit ); virtual;
       function RayCastChilds( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean; virtual;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property World       :TRayWorld     read GetWorld                           ;
       property LocalMatrix :TSingleM4     read GetLocalMatrix write SetLocalMatrix;
       property LocalMatriI :TSingleM4     read GetLocalMatriI write SetLocalMatriI;
       property WorldMatrix :TSingleM4     read GetWorldMatrix write SetWorldMatrix;
       property WorldMatriI :TSingleM4     read GetWorldMatriI write SetWorldMatriI;
       property LocalAABB   :TSingleArea3D read GetLocalAABB                       ;
       property WorldAABB   :TSingleArea3D read GetWorldAABB                       ;
       property Material    :TRayMaterial  read GetMaterial    write SetMaterial   ;
       ///// メソッド
       function HitBoundBox( const WorldRay_:TRayRay; out Len_:TSingleArea ) :Boolean;
       function RayCast( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean; virtual;
       function RayCasts( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean; virtual;
       function RayJoin( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean;
       function RayJoins( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean;
       function Raytrace( var WorldRay_:TRayRay ) :TSingleRGB; virtual;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayCamera

     TRayCamera = class( TRayGeometry )
     private
     protected
       _ScreenX :Single;
       _ScreenY :Single;
       _ScreenZ :Single;
       _ScreenW :Single;
       _ScreenH :Single;
       _AspectW :Integer;
       _AspectH :Integer;
       ///// アクセス
       procedure SetAspectW( const AspectW_:Integer );
       procedure SetAspectH( const AspectH_:Integer );
       procedure SetScreenX( const ScreenX_:Single );
       procedure SetScreenY( const ScreenY_:Single );
       procedure SetScreenZ( const ScreenZ_:Single );
       procedure SetScreenW( const ScreenW_:Single );
       procedure SetScreenH( const ScreenH_:Single );
       function GetAngleW :Single;
       procedure SetAngleW( const AngleW_:Single );
       function GetAngleH :Single;
       procedure SetAngleH( const AngleH_:Single );
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property AspectW :Integer read   _AspectW write SetAspectW;
       property AspectH :Integer read   _AspectH write SetAspectH;
       property ScreenX :Single  read   _ScreenX write SetScreenX;
       property ScreenY :Single  read   _ScreenY write SetScreenY;
       property ScreenZ :Single  read   _ScreenZ write SetScreenZ;
       property ScreenW :Single  read   _ScreenW write SetScreenW;
       property ScreenH :Single  read   _ScreenH write SetScreenH;
       property AngleW  :Single  read GetAngleW  write SetAngleW ;
       property AngleH  :Single  read GetAngleH  write SetAngleH ;
       ///// メソッド
       function Shoot( const X_,Y_:Single ) :TRayRay;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayLight

     TRayLight = class( TRayGeometry )
     private
     protected
       _Color :TSingleRGB;
       ///// メソッド
       procedure _RayJoin( var LocalRay_:TRayRay; var LocalHit_:TRayHit ); override;
     public
       constructor Create; override;
       constructor Create( const Paren_:TTreeNode ); override;
       destructor Destroy; override;
       ///// プロパティ
       property Color :TSingleRGB read _Color write _Color;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayWorld

     TRayWorld = class( TRayGeometry )
     private
     protected
       _Lights  :TArray<TRayLight>;
       _RecursN :Integer;
       ///// アクセス
       function GetWorld :TRayWorld; override;
       function GetLocalMatrix :TSingleM4; override;
       function GetLocalMatriI :TSingleM4; override;
       function GetWorldMatrix :TSingleM4; override;
       function GetWorldMatriI :TSingleM4; override;
       function GetLocalAABB :TSingleArea3D; override;
       function GetLightsN :Integer;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property LocalMatrix :TSingleM4         read GetLocalMatrix           ;
       property LocalMatriI :TSingleM4         read GetLocalMatriI           ;
       property WorldMatrix :TSingleM4         read GetWorldMatrix           ;
       property WorldMatriI :TSingleM4         read GetWorldMatriI           ;
       property Lights      :TArray<TRayLight> read   _Lights                ;
       property LightsN     :Integer           read GetLightsN               ;
       property RecursN     :Integer           read   _RecursN write _RecursN;
       ///// メソッド
       function RayCasts( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayMaterial

     TRayMaterial = class
     private
     protected
       _Geometry :TRayGeometry;
       ///// アクセス
       function GetWorld :TRayWorld; virtual;
     public
       constructor Create;
       destructor Destroy; override;
       ///// プロパティ
       property World :TRayWorld read GetWorld;
       ///// メソッド
       function Scatter( const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB; virtual; abstract;
     end;

const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

      _EPSILON_ = 0.001;

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils,
     LUX.Raytrace.Material;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayRay

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

function TRayRay.GetTip :TSingle3D;
begin
     Result := Ray.GoPos( Len );
end;

procedure TRayRay.SetTip( const Tip_:TSingle3D );
begin
     Ray.Vec := Ray.Pos.VectorTo( Tip_ );
     Len     := Ray.Vec.Size;
     Ray.Vec := Ray.Vec / Len;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayHit

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TRayHit.GetPos :TSingle3D;
begin
     Result := Ray.Tip;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

function TRayHit.Scatter( const WorldRay_:TRayRay ) :TSingleRGB;
begin
     Result := Obj.Material.Scatter( WorldRay_, Self );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayGeometry

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TRayGeometry.GetWorld :TRayWorld;
begin
     Result := Paren.World;
end;

//------------------------------------------------------------------------------

function TRayGeometry.GetLocalMatrix :TSingleM4;
begin
     if up_LocalMatrix then
     begin
          _LocalMatrix := _LocalMatriI.Inverse;

          up_LocalMatrix := False;
     end;

     Result := _LocalMatrix;
end;

procedure TRayGeometry.SetLocalMatrix( const LocalMatrix_:TSingleM4 );
begin
     _LocalMatrix := LocalMatrix_;

     up_LocalMatrix := False;  up_LocalMatriI := True ;
     up_WorldMatrix := True ;  up_WorldMatriI := True ;

     up_WorldAABB := True ;
end;

function TRayGeometry.GetLocalMatriI :TSingleM4;
begin
     if up_LocalMatriI then
     begin
          _LocalMatriI := _LocalMatrix.Inverse;

          up_LocalMatriI := False;
     end;

     Result := _LocalMatriI;
end;

procedure TRayGeometry.SetLocalMatriI( const LocalMatriI_:TSingleM4 );
begin
     _LocalMatriI := LocalMatriI_;

     up_LocalMatrix := True ;  up_LocalMatriI := False;
     up_WorldMatrix := True ;  up_WorldMatriI := True ;

     up_WorldAABB := True ;
end;

//------------------------------------------------------------------------------

function TRayGeometry.GetWorldMatrix :TSingleM4;
begin
     if up_WorldMatrix then
     begin
          _WorldMatrix := Paren.WorldMatrix * LocalMatrix;

          up_WorldMatrix := False;
     end;

     Result := _WorldMatrix;
end;

procedure TRayGeometry.SetWorldMatrix( const WorldMatrix_:TSingleM4 );
begin
     _WorldMatrix :=                     WorldMatrix_;
     _LocalMatrix := Paren.WorldMatriI * WorldMatrix_;

     up_LocalMatrix := False;  up_LocalMatriI := True ;
     up_WorldMatrix := False;  up_WorldMatriI := True ;

     up_WorldAABB := True ;
end;

function TRayGeometry.GetWorldMatriI :TSingleM4;
begin
     if up_WorldMatriI then
     begin
          _WorldMatriI := LocalMatriI * Paren.WorldMatriI;

          up_WorldMatriI := False;
     end;

     Result := _WorldMatriI;
end;

procedure TRayGeometry.SetWorldMatriI( const WorldMatriI_:TSingleM4 );
begin
     _WorldMatriI := WorldMatriI_                    ;
     _LocalMatriI := WorldMatriI_ * Paren.WorldMatrix;

     up_LocalMatrix := True ;  up_LocalMatriI := False;
     up_WorldMatrix := True ;  up_WorldMatriI := False;

     up_WorldAABB := True ;
end;

//------------------------------------------------------------------------------

function TRayGeometry.GetLocalAABB :TSingleArea3D;
begin
     Result := TSingleArea3D.PoInf;
end;

function TRayGeometry.GetWorldAABB :TSingleArea3D;
var
   B :TSingleArea3D;
   I :Integer;
begin
     if up_WorldAABB then
     begin
          B := GetLocalAABB;

          _WorldAABB := TSingleArea3D.NeInf;

          for I := 0 to 7 do
          begin
               with WorldMatrix.MultPos( B.Poin[ I ] ) do
               begin
                    with _WorldAABB do
                    begin
                         if X < Min.X then Min.X := X;
                         if Y < Min.Y then Min.Y := Y;
                         if Z < Min.Z then Min.Z := Z;

                         if X > Max.X then Max.X := X;
                         if Y > Max.Y then Max.Y := Y;
                         if Z > Max.Z then Max.Z := Z;
                    end;
               end;
          end;

          up_WorldAABB := False;
     end;

     Result := _WorldAABB;
end;

//------------------------------------------------------------------------------

function TRayGeometry.GetMaterial :TRayMaterial;
begin
     if Assigned( _Material ) then Result :=      _Material
                              else Result := Paren.Material;
end;

procedure TRayGeometry.SetMaterial( const Material_:TRayMaterial );
begin
     _Material := Material_;

     _Material._Geometry := Self;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TRayGeometry._RayCast( var LocalRay_:TRayRay; var LocalHit_:TRayHit );
begin

end;

procedure TRayGeometry._RayJoin( var LocalRay_:TRayRay; var LocalHit_:TRayHit );
begin

end;

function TRayGeometry.RayCastChilds( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean;
var
   I :Integer;
begin
     Result := False;

     for I := 0 to ChildsN-1
     do Result := Childs[ I ].RayCasts( WorldRay_, WorldHit_ ) or Result;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayGeometry.Create;
begin
     inherited;

     _WorldAABB   := GetLocalAABB      ;  up_WorldAABB   := False;

     _LocalMatrix := TSingleM4.Identify;  up_LocalMatrix := False;
     _LocalMatriI := TSingleM4.Identify;  up_LocalMatriI := False;
     _WorldMatrix := TSingleM4.Identify;  up_WorldMatrix := True ;
     _WorldMatriI := TSingleM4.Identify;  up_WorldMatriI := True ;

     _Material := nil;
end;

destructor TRayGeometry.Destroy;
begin
     if Assigned( _Material ) then _Material.Free;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TRayGeometry.HitBoundBox( const WorldRay_:TRayRay; out Len_:TSingleArea ) :Boolean;
//････････････････････････････････････････････････････････････････････････
     procedure Slab( const Min_,Max_,Pos_,Vec_:Single );
     var
        T0, T1 :Single;
     begin
          T0 := ( Min_ - Pos_ ) / Vec_;
          T1 := ( Max_ - Pos_ ) / Vec_;

          with Len_ do
          begin
               if Min < T0 then Min := T0;
               if T1 < Max then Max := T1;
          end;
     end;
//････････････････････････････････････････････････････････････････････････
begin
     Len_ := TSingleArea.PoMax;

     with WorldRay_.Ray, WorldAABB do
     begin
          if Vec.X > 0 then Slab( Min.X, Max.X, Pos.X, Vec.X )
                       else
          if Vec.X < 0 then Slab( Max.X, Min.X, Pos.X, Vec.X );

          if Vec.Y > 0 then Slab( Min.Y, Max.Y, Pos.Y, Vec.Y )
                       else
          if Vec.Y < 0 then Slab( Max.Y, Min.Y, Pos.Y, Vec.Y );

          if Vec.Z > 0 then Slab( Min.Z, Max.Z, Pos.Z, Vec.Z )
                       else
          if Vec.Z < 0 then Slab( Max.Z, Min.Z, Pos.Z, Vec.Z );
     end;

     with Len_ do Result := ( Min <= Max );
end;

function TRayGeometry.RayCast( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean;
var
   L :TSingleArea;
   A :TRayRay;
   H :TRayHit;
begin
     Result := HitBoundBox( WorldRay_, L );

     if Result then
     begin
          with A do
          begin
               Emt := nil;
               Ord := WorldRay_.Ord;
               Ray := WorldMatriI * WorldRay_.Ray;
             //Len
               Hit := @H;
          end;

          with H do
          begin
               Ray := @A;
               Obj := nil;
             //Nor
             //Tan
             //Bin
             //Tex
          end;

          _RayCast( A, H );

          Result := Assigned( H.Obj ) and ( A.Len < WorldRay_.Len );

          if Result then
          begin
               with WorldRay_ do
               begin
                  //Emt
                  //Ord
                  //Ray
                    Len := A.Len;
                  //Hit
               end;

               with WorldHit_ do
               begin
                  //Ray
                    Obj :=                                      H.Obj         ;
                    Nor := H.Obj.WorldMatriI.Transpose.MultVec( H.Nor ).Unitor;
                    Tan := H.Obj.WorldMatriI.Transpose.MultVec( H.Tan ).Unitor;
                    Bin := H.Obj.WorldMatriI.Transpose.MultVec( H.Bin ).Unitor;
                    Tex :=                                      H.Tex         ;
               end;
          end;
     end;
end;

function TRayGeometry.RayCasts( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean;
begin
     Result := RayCast( WorldRay_, WorldHit_ );

     Result := RayCastChilds( WorldRay_, WorldHit_ ) or Result;
end;

function TRayGeometry.RayJoin( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean;
var
   A :TRayRay;
   H :TRayHit;
begin
     with A do
     begin
          Emt     := nil;
          Ord     :=                      WorldRay_.Ord      ;
          Ray.Pos := WorldMatriI.MultPos( WorldRay_.Ray.Pos );
        //Ray.Vec
        //Len
          Hit     := @H;
     end;

     with H do
     begin
          Ray := @A;
          Obj := nil;
        //Nor
        //Tan
        //Bin
        //Tex
     end;

     _RayJoin( A, H );

     Result := Assigned( H.Obj );

     if Result then
     begin
          with WorldRay_ do
          begin
             //Emt
             //Ord
             //Ray.Pos
               Ray.Vec := WorldMatrix.MultVec( A.Ray.Vec );
               Len     :=                      A.Len      ;
             //Hit
          end;

          with WorldHit_ do
          begin
             //Ray
               Obj :=                                      H.Obj         ;
               Nor := H.Obj.WorldMatriI.Transpose.MultVec( H.Nor ).Unitor;
               Tan := H.Obj.WorldMatriI.Transpose.MultVec( H.Tan ).Unitor;
               Bin := H.Obj.WorldMatriI.Transpose.MultVec( H.Bin ).Unitor;
               Tex :=                                      H.Tex         ;
          end;
     end;
end;

function TRayGeometry.RayJoins( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean;
begin
     Result := RayJoin( WorldRay_, WorldHit_ ) and not World.RayCasts( WorldRay_, WorldHit_ );
end;

function TRayGeometry.Raytrace( var WorldRay_:TRayRay ) :TSingleRGB;
var
   H :TRayHit;
begin
     if WorldRay_.Ord <= World.RecursN then
     begin
          with H do
          begin
               Ray := @WorldRay_;
               Obj := nil;
            //_Nor
            //_Tan
            //_Bin
            // Tex
          end;

          if RayCasts( WorldRay_, H ) then Result := H.Scatter( WorldRay_ )
                                      else Result := 0;
     end
     else Result := 0;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayCamera

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

procedure TRayCamera.SetAspectW( const AspectW_:Integer );
begin
     _AspectW := AspectW_;
end;

procedure TRayCamera.SetAspectH( const AspectH_:Integer );
begin
     _AspectH := AspectH_;
end;

//------------------------------------------------------------------------------

procedure TRayCamera.SetScreenX( const ScreenX_:Single );
begin
     _ScreenX := ScreenX_;
end;

procedure TRayCamera.SetScreenY( const ScreenY_:Single );
begin
     _ScreenY := ScreenY_;
end;

procedure TRayCamera.SetScreenZ( const ScreenZ_:Single );
begin
     _ScreenZ := ScreenZ_;
end;

procedure TRayCamera.SetScreenW( const ScreenW_:Single );
begin
     _ScreenW := ScreenW_;
end;

procedure TRayCamera.SetScreenH( const ScreenH_:Single );
begin
     _ScreenH := ScreenH_;
end;

//------------------------------------------------------------------------------

function TRayCamera.GetAngleW :Single;
begin
     Result := 2 * ArcTan( _ScreenW / 2 / _ScreenZ );
end;

procedure TRayCamera.SetAngleW( const AngleW_:Single );
begin
     _ScreenW := 2 * _ScreenZ * ArcTan( AngleW_ / 2 );
end;

function TRayCamera.GetAngleH :Single;
begin
     Result := 2 * ArcTan( _ScreenH / 2 / _ScreenZ );
end;

procedure TRayCamera.SetAngleH( const AngleH_:Single );
begin
     _ScreenH := 2 * _ScreenZ * ArcTan( AngleH_ / 2 );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayCamera.Create;
begin
     inherited;

     _ScreenX := 0;
     _ScreenY := 0;
     _ScreenZ := 4;
     _ScreenW := 4;
     _ScreenH := 3;
     _AspectW := 1;
     _AspectH := 1;
end;

destructor TRayCamera.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TRayCamera.Shoot( const X_,Y_:Single ) :TRayRay;
begin
     with Result do
     begin
          Emt := nil;

          Ord     := 1;

          Ray.Pos := TSingle3D.Create( 0, 0, 0 );

          with Ray.Vec do
          begin
               X := -_ScreenW/2 + _ScreenW * X_;
               Y := +_ScreenH/2 - _ScreenH * Y_;
               Z := -_ScreenZ                  ;
          end;

          Ray := TSingleRay3D( WorldMatrix * Ray.Unitor );

          Len := Single.PositiveInfinity;

          Hit := nil;
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayLight

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

procedure TRayLight._RayJoin( var LocalRay_:TRayRay; var LocalHit_:TRayHit );
begin
     with LocalHit_ do
     begin
        //Ray
          Obj := Self;
        //Nor
        //Tan
        //Bin
        //Tex
     end;

     with LocalRay_ do
     begin
        //Emt
        //Ord
        //Ray
        //Len
        //Hit
          Tip := TSingle3D.Create( 0, 0, 0 );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayLight.Create;
begin
     inherited;

end;

constructor TRayLight.Create( const Paren_:TTreeNode );
begin
     inherited;

     World._Lights := World._Lights + [ Self ];
end;

destructor TRayLight.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayWorld

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TRayWorld.GetWorld :TRayWorld;
begin
     Result := Self;
end;

//------------------------------------------------------------------------------

function TRayWorld.GetLocalMatrix :TSingleM4;
begin
     Result := TSingleM4.Identify;
end;

function TRayWorld.GetLocalMatriI :TSingleM4;
begin
     Result := TSingleM4.Identify;
end;

function TRayWorld.GetWorldMatrix :TSingleM4;
begin
     Result := TSingleM4.Identify;
end;

function TRayWorld.GetWorldMatriI :TSingleM4;
begin
     Result := TSingleM4.Identify;
end;

//------------------------------------------------------------------------------

function TRayWorld.GetLocalAABB :TSingleArea3D;
begin
     Result := TSingleArea3D.NeInf;
end;

//------------------------------------------------------------------------------

function TRayWorld.GetLightsN :Integer;
begin
     Result := Length( _Lights );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayWorld.Create;
begin
     inherited;

     _Material := TMaterialRGB.Create;
     _Lights   := [];
     _RecursN  := 8;
end;

destructor TRayWorld.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TRayWorld.RayCasts( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean;
begin
     Result := RayCastChilds( WorldRay_, WorldHit_ );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMaterial

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

function TRayMaterial.GetWorld :TRayWorld;
begin
     Result := _Geometry.World;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayMaterial.Create;
begin
     inherited;

end;

destructor TRayMaterial.Destroy;
begin

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■