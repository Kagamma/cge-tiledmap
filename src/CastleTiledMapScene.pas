unit CastleTiledMapScene;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Castle3D, Castle2DSceneManager,
  CastleVectors, CastleGenericLists,
  CastleTimeUtils,
  CastleTiledMap,
  X3DFields, X3DTime, X3DNodes,
  fgl;

type
  PCastleTile = ^TCastleTile;
  TCastleTile = record
    GID,
    CoordIndex: cardinal;
    ShapeNode: TShapeNode;
  end;
  TCastleTiles = array of TCastleTile;
  TShapeNodes = array of TShapeNode;
  TShapeNodeMap = specialize TFPGMap<string, TShapeNode>;
  TImageTexNodeMap = specialize TFPGMap<string, TImageTextureNode>;

  PCastleTiledGeoLayer = ^TCastleTiledGeoLayer;
  TCastleTiledGeoLayer = record
    { Name of layer, for searching. }
    Name: string;
    { Array of tiles available for this layer. }
    Tiles: TCastleTiles;
    { Each layer can contain more than 1 shape, as tmx allows to use multiple
      tilesets per layer. }
    ShapeNodes: TShapeNodes;
  end;
  TCastleTiledGeoLayerList = specialize TGenericStructList<TCastleTiledGeoLayer>;

  { Scene contain tiled map, split into blocks for faster rendering. }
  TCastleTiledMapScene = class(T2DScene)
  private
    FURL: string;
    FTiledMap: TTiledMap;
    { Size per partition, default is 128. They should be changed before calling
      LoadTMX(). }
    FTilePartWidth,
    FTilePartHeight: word;
    { List of geometry layer, used internally by the class to manipulate
      shapes. }
    FTiledGeoLayerList: TCastleTiledGeoLayerList;
                           
    procedure InitNodeTree;
    { Calculate the exact texture coords of frame from a given frame. }
    procedure CalculateFrameCoords(
        const AWidth, AHeight, AFrameWidth, AFrameHeight, AFrame: cardinal;
        const AData: cardinal;
        out T1, T2, T3, T4: TVector2Single);
    { Check ShapeNodeMap to see if there's a shape for a given key yet, if not,
      create new one for it.       
      The method returns shape if existed, or create a new one and then return.
      @param(AParentNode) - Parent of the new shape, in case it doesnt exist.
      @param(AShapeNodeMap) - Map contains all shapes of a layer.
      @param(AImageTexNodeMap) - Map contains image for reusing purpose.
      @param(AImageName) - Name of the image.
      @param(APartSignature) - Partition signature, in "[x-y]" format.
      @param(AGeoLayer) - }
    function CheckExistedShape(
        const AParentNode: TAbstractGroupingNode;
        const AShapeNodeMap: TShapeNodeMap;
        const AImageTexNodeMap: TImageTexNodeMap;
        const AImageName, APartSignature: string;
        const AGeoLayer: PCastleTiledGeoLayer): TShapeNode;
    { Construct an orthogonal map from given tiled map. }
    procedure ConstructOrthogonalMap(const AParentNode: TAbstractGroupingNode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadTMX(const AURL: string);
    { Delete a tile. Note that this only delete from X3D and it does not make
      any changes on TiledMap. You need to manually delete it from TiledMap. }
    procedure DeleteTile(const ALayerName: string; const TileX, TileY: cardinal);

    property TiledMap: TTiledMap read FTiledMap;
    property TilePartWidth: word read FTilePartWidth write FTilePartWidth;
    property TilePartHeight: word read FTilePartHeight write FTilePartHeight;
  end;

implementation

uses
  Math,
  CastleFilesUtils, CastleSceneCore, CastleConfig,  
  CastleDownload, CastleURIUtils;

const
  VERTEX_BUFFER = 4096;
  HorizontalFlag = $80000000;
  VerticalFlag = $40000000;
  DiagonalFlag = $20000000;
  ClearFlag = $1FFFFFFF;

procedure TCastleTiledMapScene.InitNodeTree;
var
  Root: TX3DRootNode;
  ColNode: TCollisionNode;
begin
  Root := TX3DRootNode.Create;
  { No collisions for geometry. Collision will be handled by proximy instead. }
  ColNode := TCollisionNode.Create;
  ColNode.Enabled := false;
  Root.FdChildren.Add(ColNode);
  { Construct tile batches from layers. }
  case FTiledMap.Orientation of
    MO_Orthogonal:
      ConstructOrthogonalMap(ColNode);
  end;
  Self.Load(Root, true);
end;

procedure TCastleTiledMapScene.CalculateFrameCoords(
    const AWidth, AHeight, AFrameWidth, AFrameHeight, AFrame: cardinal;
    const AData: cardinal;
    out T1, T2, T3, T4: TVector2Single);
var
  W, H,
  Columns: integer;
  FW, FH,
  FFW, FFH: single;

  procedure SwapVector(var V1, V2: TVector2Single);
  var
    V: TVector2Single;
  begin
    V := V1; V1 := V2; V2 := V;
  end;

begin
  Columns := AWidth div AFrameWidth;
  W := AFrame mod Columns * AFrameWidth;
  H := (AHeight - AFrameHeight) - AFrame div Columns * AFrameHeight;
  FW := 1 / AWidth * W;
  FH := 1 / AHeight * H;
  FFW := 1 / AWidth * AFrameWidth;
  FFH := 1 / AHeight * AFrameHeight;
  T1 := Vector2Single(FW, FH + FFH);
  T2 := Vector2Single(FW + FFW, FH + FFH);
  T3 := Vector2Single(FW + FFW, FH);
  T4 := Vector2Single(FW, FH);
  if (AData and HorizontalFlag) > 0 then
  begin
    SwapVector(T1, T2);
    SwapVector(T3, T4);
  end;                       
  if (AData and VerticalFlag) > 0 then
  begin
    SwapVector(T1, T4);
    SwapVector(T2, T3);
  end;
end;

function TCastleTiledMapScene.CheckExistedShape(
    const AParentNode: TAbstractGroupingNode;
    const AShapeNodeMap: TShapeNodeMap; 
    const AImageTexNodeMap: TImageTexNodeMap;
    const AImageName, APartSignature: string;
    const AGeoLayer: PCastleTiledGeoLayer): TShapeNode;
var
  Key: string;
  i: integer;
  ShapeNode: TShapeNode;
  ImageTexNode: TImageTextureNode;
  TexAttrNode: TTexturePropertiesNode;
  TriSetNode: TTriangleSetNode;
  CoordNode: TCoordinateNode;
  TexCoordNode: TTextureCoordinateNode;
begin
  Key := AImageName + APartSignature;
  if not AShapeNodeMap.TryGetData(Key, ShapeNode) then
  begin
    ShapeNode := TShapeNode.Create(AImageName);

    i := Length(AGeoLayer^.ShapeNodes);
    SetLength(AGeoLayer^.ShapeNodes, i+1);
    AGeoLayer^.ShapeNodes[i] := ShapeNode;
    AShapeNodeMap[Key] := ShapeNode;

    (AParentNode as TGroupNode).FdChildren.Add(ShapeNode);

    ShapeNode.Appearance := TAppearanceNode.Create;
    ShapeNode.Material := TMaterialNode.Create;

    ShapeNode.Material.DiffuseColor := Vector3Single(0, 0, 0);
    ShapeNode.Material.SpecularColor := Vector3Single(0, 0, 0);
    ShapeNode.Material.AmbientIntensity := 0;
    ShapeNode.Material.EmissiveColor := Vector3Single(1, 1, 1);

    if not AImageTexNodeMap.TryGetData(AImageName, ImageTexNode) then
    begin
      ImageTexNode := TImageTextureNode.Create;
      ImageTexNode.RepeatS := false;
      ImageTexNode.RepeatT := false;
      ImageTexNode.FdUrl.Send(ExtractURIPath(FURL) + AImageName);
      TexAttrNode:= TTexturePropertiesNode.Create;
      TexAttrNode.FdMagnificationFilter.Send('NEAREST_PIXEL');
      TexAttrNode.FdMinificationFilter.Send('NEAREST_PIXEL');
      ImageTexNode.FdTextureProperties.Value := TexAttrNode;

      AImageTexNodeMap[AImageName] := ImageTexNode;
    end;
    ShapeNode.Texture := ImageTexNode;

    TriSetNode := TTriangleSetNode.Create;
    TriSetNode.Solid := false;
    ShapeNode.FdGeometry.Value := TriSetNode;

    CoordNode := TCoordinateNode.Create;
    TexCoordNode := TTextureCoordinateNode.Create;

    TriSetNode.FdCoord.Value := CoordNode;
    TriSetNode.FdTexCoord.Value := TexCoordNode;

    CoordNode.FdPoint.Items.Capacity := VERTEX_BUFFER;
    TexCoordNode.FdPoint.Items.Capacity := VERTEX_BUFFER;
  end;
  Result := ShapeNode;
end;

procedure TCastleTiledMapScene.ConstructOrthogonalMap(const AParentNode: TAbstractGroupingNode);
var
  GID,
  Data: cardinal;
  i, j, k, c: integer;
  TileWidth,
  TileHeight,
  CurTilePartX,
  CurTilePartY: integer;

  Tileset_: PTileset;
  Layer_: PLayer;
  Tile_: PCastleTile;
  GeoLayer_: PCastleTiledGeoLayer;

  GroupNode: TGroupNode;
  ShapeNode: TShapeNode;
  TriSetNode: TTriangleSetNode;
  CoordNode: TCoordinateNode;
  TexCoordNode: TTextureCoordinateNode;
  ShapeNodeMap: TShapeNodeMap;
  ImageTexNodeMap: TImageTexNodeMap;

  V1, V2, V3, V4: TVector3Single;
  T1, T2, T3, T4: TVector2Single;

  W, H: single;
  ZDepth: single = 0;

begin
  FTiledGeoLayerList.Count := FTiledMap.Layers.Count;
  ShapeNodeMap := TShapeNodeMap.Create;
  ShapeNodeMap.Sorted := true;
  ImageTexNodeMap := TImageTexNodeMap.Create;
  ImageTexNodeMap.Sorted := true;
  TileWidth := FTiledMap.TileWidth;     
  TileHeight := FTiledMap.TileHeight;
  try
    for i := 0 to FTiledMap.Layers.Count-1 do
    begin
      Layer_ := FTiledMap.Layers.Ptr(i);
      if (Layer_^.LayerType = LT_ObjectGroup) or
         (Layer_^.LayerType = LT_ImageLayer) then
        continue;
      if not Layer_^.Visible then
        continue;

      ShapeNodeMap.Clear;    
      GeoLayer_ := FTiledGeoLayerList.Ptr(i);
      GeoLayer_^.Name := Layer_^.Name;
      SetLength(GeoLayer_^.Tiles, FTiledMap.Width * FTiledMap.Height);
      GroupNode := TGroupNode.Create(Layer_^.Name);
      (AParentNode as TCollisionNode).FdChildren.Add(GroupNode);

      c := 0;
      for k := 0 to FTiledMap.Height-1 do
      begin                      
        CurTilePartY := k div FTilePartHeight;
        for j := 0 to FTiledMap.Width-1 do
        begin
          CurTilePartX := j div FTilePartWidth;
          Data := Layer_^.Data.Data[c];
          Inc(c);
          GID := Data and ClearFlag;
          if GID = 0 then
            continue;
          Tileset_ := FTiledMap.GIDToTileset(GID);
          ShapeNode := CheckExistedShape(
              GroupNode, ShapeNodeMap, ImageTexNodeMap,
              Tileset_^.Image.Source,
              '[' + CurTilePartX.ToString + 'x' + CurTilePartY.ToString + ']',
              GeoLayer_);

          TriSetNode := ShapeNode.FdGeometry.Value as TTriangleSetNode;
          CoordNode := TriSetNode.FdCoord.Value as TCoordinateNode;
          TexCoordNode :=TriSetNode.FdTexCoord.Value as TTextureCoordinateNode;

          W := j; H := k;
          V1 := Vector3Single(W * TileWidth, H * TileHeight, ZDepth);
          V2 := Vector3Single((W + 1) * TileWidth, H * TileHeight, ZDepth);
          V3 := Vector3Single((W + 1) * TileWidth, (H + 1) * TileHeight, ZDepth);
          V4 := Vector3Single(W * TileWidth, (H + 1) * TileHeight, ZDepth);

          CalculateFrameCoords(
              Tileset_^.Image.Width, Tileset_^.Image.Height,
              TileWidth, TileHeight,
              GID - Tileset_^.FirstGID, Data,
              T1, T2, T3, T4);

          Tile_ := @GeoLayer_^.Tiles[k * FTiledMap.Height + j];
          Tile_^.ShapeNode := ShapeNode;
          Tile_^.CoordIndex := CoordNode.FdPoint.Items.Count;
          Tile_^.GID := GID;

          CoordNode.FdPoint.Items.AddArray([V1, V2, V3, V1, V3, V4]);
          TexCoordNode.FdPoint.Items.AddArray([T1, T2, T3, T1, T3, T4]);
        end;
      end;   
      ZDepth += 0.1;
    end;
  finally
    FreeAndNil(ShapeNodeMap);      
    FreeAndNil(ImageTexNodeMap);
  end;
end;

constructor TCastleTiledMapScene.Create(AOwner: TComponent);
begin
  inherited;
  FTiledMap := nil;
  FTiledGeoLayerList := TCastleTiledGeoLayerList.Create;
  FTilePartWidth := 128;
  FTilePartHeight := 128;
end;

destructor TCastleTiledMapScene.Destroy;
begin
  FreeAndNil(FTiledMap);
  FTiledGeoLayerList.Free;
  inherited;
end;                 

procedure TCastleTiledMapScene.DeleteTile(const ALayerName: string; const TileX, TileY: cardinal);
var
  i, j, c: integer;
  GeoLayer_: PCastleTiledGeoLayer;
  GeoTile_: PCastleTile;
  TriSetNode: TTriangleSetNode;
  CoordList: TVector3SingleList;   
  TexCoordList: TVector2SingleList;
begin
  for i := 0 to FTiledGeoLayerList.Count-1 do
  begin
    GeoLayer_ := FTiledGeoLayerList.Ptr(i);
    if GeoLayer_^.Name = ALayerName then
    begin
      c := TileY * FTiledMap.Width + TileX - 1;
      GeoTile_ := @GeoLayer_^.Tiles[c];
      if not Assigned(GeoTile_^.ShapeNode) then
        exit;
      TriSetNode := GeoTile_^.ShapeNode.FdGeometry.Value as TTriangleSetNode;
      CoordList := (TriSetNode.FdCoord.Value as TCoordinateNode).FdPoint.Items;
      TexCoordList := (TriSetNode.FdTexCoord.Value as TTextureCoordinateNode).FdPoint.Items;
      { Remove 6 vertices from shape. }
      for j := 0 to 5 do
      begin
        CoordList.Delete(GeoTile_^.CoordIndex);
        TexCoordList.Delete(GeoTile_^.CoordIndex);
      end;
      { We need to loop through the tiles to dec CoordIndex. }
      for j := c+1 to High(GeoLayer_^.Tiles) do
      begin
        if GeoLayer_^.Tiles[j].ShapeNode = GeoTile_^.ShapeNode then
          Dec(GeoLayer_^.Tiles[j].CoordIndex, 6);
      end;
      TriSetNode.FdCoord.Changed;
      TriSetNode.FdTexCoord.Changed;
      break;
    end;
  end;
end;

procedure TCastleTiledMapScene.LoadTMX(const AURL: string);
begin
  FreeAndNil(FTiledMap);
  FTiledMap := TTiledMap.Create(AURL);
  FURL := AURL;

  InitNodeTree;
end;

end.

