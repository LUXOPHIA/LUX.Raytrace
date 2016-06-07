unit LUX.Raytrace;

interface //#################################################################### ■

uses LUX, LUX.D3, LUX.Matrix.L4, LUX.Color, LUX.Graph.Tree;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     TRayHit      = class;
     TRayGeometry = class;
       TRayCamera = class;
       TRayLight  = class;
       TRayWorld  = class;
     TRayMaterial = class;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRadiance

     TRadiance = record
     private
     public
       Ray :TSingleRay3D;
       Wei :TSingleRGB;
       Col :TSingleRGB;
     end;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayHit

     TRayHit = class
     private
     protected
       ///// アクセス
       function GetObj :TRayGeometry;
       function GetLen :Single;
       function GetPos :TSingle3D;
     public
       _Obj :TRayGeometry;
       _Len :Single;
       _Pos :TSingle3D;
       ///// プロパティ
       property Obj :TRayGeometry read GetObj;
       property Len :Single       read GetLen;
       property Pos :TSingle3D    read GetPos;
     end;

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
       _LocalAABB   :TSingleArea3D;
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
       procedure SetLocalAABB( const LocalAABB_:TSingleArea3D );
       function GetWorldAABB :TSingleArea3D; virtual;
       function GetMaterial :TRayMaterial; virtual;
       procedure SetMaterial( const Material_:TRayMaterial ); virtual;
       ///// メソッド
       function _RayCast( const LocalRay_:TSingleRay3D ) :TRayHit; virtual;
       function _RayJoin( const LocalPos_:TSingle3D ) :TRayHit; virtual;
       procedure RayCastChilds( const WorldRay_:TSingleRay3D; var Hit_:TRayHit ); virtual;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property World       :TRayWorld     read GetWorld                           ;
       property LocalMatrix :TSingleM4     read GetLocalMatrix write SetLocalMatrix;
       property LocalMatriI :TSingleM4     read GetLocalMatriI write SetLocalMatriI;
       property WorldMatrix :TSingleM4     read GetWorldMatrix write SetWorldMatrix;
       property WorldMatriI :TSingleM4     read GetWorldMatriI write SetWorldMatriI;
       property LocalAABB   :TSingleArea3D read   _LocalAABB   write SetLocalAABB  ;
       property WorldAABB   :TSingleArea3D read GetWorldAABB                       ;
       property Material    :TRayMaterial  read GetMaterial    write SetMaterial   ;
       ///// メソッド
       function HitBoundBox( const WorldRay_:TSingleRay3D ) :Boolean; virtual;
       function RayCast( const WorldRay_:TSingleRay3D ) :TRayHit; virtual;
       function RayCasts( const WorldRay_:TSingleRay3D ) :TRayHit; virtual;
       function RayJoin( const WorldPos_:TSingle3D ) :TRayHit;
       function RayJoins( const WorldPos_:TSingle3D ) :TRayHit; virtual;
       function Raytrace( const WorldRay_:TSingleRay3D; const RayN_:Integer ) :TSingleRGB; virtual;
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
       function Shoot( const X_,Y_:Single ) :TSingleRay3D;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayLight

     TRayLight = class( TRayGeometry )
     private
     protected
       _Color :TSingleRGB;
       ///// メソッド
       function _RayJoin( const LocalPos_:TSingle3D ) :TRayHit; override;
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
       function HitBoundBox( const WorldRay_:TSingleRay3D ) :Boolean; override;
       function RayCasts( const WorldRay_:TSingleRay3D ) :TRayHit; override;
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
       function Scatter( const WorldRay_:TSingleRay3D; const RayN_:Integer; const Hit_:TRayHit ) :TSingleRGB; virtual; abstract;
     end;

const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

      _EPSILON_ = 0.001;

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils,
     LUX.Raytrace.Material;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRadiance

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayHit

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TRayHit.GetObj :TRayGeometry;
begin
     Result := _Obj;
end;

function TRayHit.GetLen :Single;
begin
     Result := _Len;
end;

function TRayHit.GetPos :TSingle3D;
begin
     Result := _Obj.WorldMatrix.MultPos( _Pos );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayGeometry

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TRayGeometry.GetWorld :TRayWorld;
begin
     Result := Paren.World;
end;

//------------------------------------------------------------------------------

procedure TRayGeometry.SetLocalAABB( const LocalAABB_:TSingleArea3D );
begin
     _LocalAABB := LocalAABB_;

     up_WorldAABB := True;
end;

function TRayGeometry.GetWorldAABB :TSingleArea3D;
var
   I :Integer;
begin
     if up_WorldAABB then
     begin
          _WorldAABB := TSingleArea3D.MinArea;

          for I := 0 to 7 do
          begin
               with WorldMatrix.MultPos( LocalAABB.Poin[ I ] ) do
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

function TRayGeometry._RayCast( const LocalRay_:TSingleRay3D ) :TRayHit;
begin
     Result := nil;
end;

function TRayGeometry._RayJoin( const LocalPos_:TSingle3D ) :TRayHit;
begin
     Result := nil;
end;

procedure TRayGeometry.RayCastChilds( const WorldRay_:TSingleRay3D; var Hit_:TRayHit );
var
   I :Integer;
   H :TRayHit;
begin
     for I := 0 to ChildsN-1 do
     begin
          H := Childs[ I ].RayCasts( WorldRay_ );

          if Assigned( Hit_ ) then
          begin
               if Assigned( H ) then
               begin
                    if H.Len < Hit_.Len then
                    begin
                         Hit_.Free;

                         Hit_ := H;
                    end
                    else H.Free;
               end;
          end
          else Hit_ := H;
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TRayGeometry.Create;
begin
     inherited;

     _LocalAABB := TSingleArea3D.MaxArea;

     _WorldAABB := _LocalAABB      ;  up_WorldAABB := False;

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

function TRayGeometry.HitBoundBox( const WorldRay_:TSingleRay3D ) :Boolean;
var
   MinT, MaxT :Single;
//････････････････････････････････････････････････････････････････････････
     procedure Slab( const Min_,Max_,Pos_,Vec_:Single );
     var
        T0, T1 :Single;
     begin
          T0 := ( Min_ - Pos_ ) / Vec_;
          T1 := ( Max_ - Pos_ ) / Vec_;

          if MinT < T0 then MinT := T0;
          if T1 < MaxT then MaxT := T1;
     end;
//････････････････････････････････････････････････････････････････････････
begin
     MinT := -Single.MaxValue;
     MaxT := +Single.MaxValue;

     with WorldRay_, WorldAABB do
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

     Result := ( MinT <= MaxT );
end;

function TRayGeometry.RayCast( const WorldRay_:TSingleRay3D ) :TRayHit;
begin
     if HitBoundBox( WorldRay_ ) then Result := _RayCast( TSingleRay3D( WorldMatriI * WorldRay_ ).Unitor )
                                 else Result := nil;
end;

function TRayGeometry.RayCasts( const WorldRay_:TSingleRay3D ) :TRayHit;
begin
     Result := RayCast( WorldRay_ );

     RayCastChilds( WorldRay_, Result );
end;

function TRayGeometry.RayJoin( const WorldPos_:TSingle3D ) :TRayHit;
begin
     Result := _RayJoin( WorldMatriI.MultPos( WorldPos_ ) );
end;

function TRayGeometry.RayJoins( const WorldPos_:TSingle3D ) :TRayHit;
var
   A :TSingleRay3D;
   H :TRayHit;
begin
     Result := RayJoin( WorldPos_ );

     if Assigned( Result ) then
     begin
          A.Pos := WorldPos_;
          A.Vec := WorldPos_.UnitorTo( Result.Pos );

          H := World.RayCasts( A );

          if Assigned( H ) then
          begin
               if H.Len < Result.Len then
               begin
                    Result.Free;

                    Result := nil;
               end;

               H.Free;
          end;
     end;
end;

function TRayGeometry.Raytrace( const WorldRay_:TSingleRay3D; const RayN_:Integer ) :TSingleRGB;
var
   H :TRayHit;
begin
     if RayN_ <= World.RecursN then
     begin
          H := RayCasts( WorldRay_ );

          if Assigned( H ) then
          begin
               Result := H.Obj.Material.Scatter( WorldRay_, RayN_ + 1, H );

               H.Free;
          end
          else Result := TSingleRGB.Create( 0, 0, 0 );
     end
     else Result := TSingleRGB.Create( 0, 0, 0 );
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

function TRayCamera.Shoot( const X_,Y_:Single ) :TSingleRay3D;
begin
     with Result do
     begin
          Pos := TSingle3D.Create( 0, 0, 0 );

          with Vec do
          begin
               X := -_ScreenW/2 + _ScreenW * X_;
               Y := +_ScreenH/2 - _ScreenH * Y_;
               Z := -_ScreenZ                  ;
          end;
     end;

     Result := TSingleRay3D( WorldMatrix * Result ).Unitor;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TRayLight

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

function TRayLight._RayJoin( const LocalPos_:TSingle3D ) :TRayHit;
begin
     Result := TRayHit.Create;

     with Result do
     begin
          _Obj := Self;
          _Pos := TSingle3D.Create( 0, 0, 0 );
          _Len := Distance( LocalPos_, _Pos );
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

function TRayWorld.HitBoundBox( const WorldRay_:TSingleRay3D ) :Boolean;
begin
     Result := False;
end;

function TRayWorld.RayCasts( const WorldRay_:TSingleRay3D ) :TRayHit;
begin
     Result := nil;

     RayCastChilds( WorldRay_, Result );
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