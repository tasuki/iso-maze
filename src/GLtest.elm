module GLtest exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onMouseDown, onMouseMove, onMouseUp, onResize)
import Html exposing (Html, a, div, text)
import Html.Attributes as Attributes exposing (href, style)
import Json.Decode as Decode exposing (Decoder)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import Task
import WebGL exposing (Mesh, Shader)


type Msg
    = Diff Float
    | MouseMove Float Float
    | Press Bool
    | Resize Float Float


type alias Model =
    { width : Float
    , height : Float
    , left : Float
    , top : Float
    , pressed : Bool
    , time : Float
    }


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( Model 0 0 0 0 False 0
                , Task.perform (\{ viewport } -> Resize viewport.width viewport.height) getViewport
                )
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Diff time ->
            ( { model | time = model.time + time }
            , Cmd.none
            )

        MouseMove left top ->
            ( { model | left = left, top = top }
            , Cmd.none
            )

        Press pressed ->
            ( { model | pressed = pressed }
            , Cmd.none
            )

        Resize width height ->
            ( { model | width = width, height = height }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions { pressed } =
    Sub.batch
        [ onResize (\w h -> Resize (toFloat w) (toFloat h))
        , onMouseDown (Decode.succeed (Press True))
        , onMouseUp (Decode.succeed (Press False))
        , if pressed then
            onMouseMove mousePosition

          else
            Sub.none
        ]


mousePosition : Decoder Msg
mousePosition =
    Decode.map2 MouseMove
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


view : Model -> Html msg
view { width, height, left, top, time } =
    div []
        [ WebGL.toHtml
            [ Attributes.width (round width)
            , Attributes.height (round height)
            , style "position" "absolute"
            , style "left" "0"
            , style "top" "0"
            , style "display" "block"
            ]
            [ WebGL.entity
                vertexShader
                fragmentShader
                mesh
                { iResolution = vec3 width height 0
                , iGlobalTime = time / 1000
                , iMouse = vec4 left top 0 0
                }
            ]
        ]



-- Mesh


mesh : Mesh { position : Vec3 }
mesh =
    WebGL.triangles
        [ ( { position = vec3 -1 1 0 }
          , { position = vec3 1 1 0 }
          , { position = vec3 -1 -1 0 }
          )
        , ( { position = vec3 -1 -1 0 }
          , { position = vec3 1 1 0 }
          , { position = vec3 1 -1 0 }
          )
        ]



-- Shaders


type alias Uniforms =
    { iResolution : Vec3
    , iGlobalTime : Float
    , iMouse : Vec4
    }


vertexShader : Shader { position : Vec3 } Uniforms { vFragCoord : Vec2 }
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        varying vec2 vFragCoord;
        uniform vec3 iResolution;

        void main () {
            gl_Position = vec4(position, 1.0);
            vFragCoord = (position.xy + 1.0) / 2.0 * iResolution.xy;
        }
    |]


fragmentShader : Shader {} Uniforms { vFragCoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec2      vFragCoord;
        uniform vec3      iResolution;           // viewport resolution (in pixels)
        uniform float     iGlobalTime;           // shader playback time (in seconds)
        uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click

const int antialias = 2;

float hash1( float n ) { return fract(sin(n)*43758.5453); }
vec2  hash2( vec2  p ) { p = vec2( dot(p,vec2(127.1,311.7)), dot(p,vec2(269.5,183.3)) ); return fract(sin(p)*43758.5453); }

float noise( in vec2 x )
{
    vec2 p = floor(x);
    vec2 f = fract(x);
    f = f*f*(3.0-2.0*f);
    float n = p.x + p.y*57.0;
    return mix(mix( hash1(n+  0.0), hash1(n+  1.0),f.x),
               mix( hash1(n+ 57.0), hash1(n+ 58.0),f.x),f.y);
}

vec3 texturef( in vec2 p )
{
	vec2 q = p;
	p = p*vec2(6.0,128.0);
	float f = 0.0;
    //f += 0.500*noise( p ); p = p*2.02;
    //f += 0.250*noise( p ); p = p*2.03;
    //f += 0.125*noise( p ); p = p*2.01;
	//f /= 0.875;

	vec3 col = 0.6 + 0.4*sin( f*2.5 + 1.0+vec3(0.0,0.5,1.0) );
	//col *= 0.7 + 0.3*noise( 8.0*q.yx );
	//col *= 0.8 + 0.2*clamp(2.0*noise(256.0*q.yx ),0.0,1.0);
    col *= vec3(1.0,0.65,0.5) * 0.85;
    return col;

}

vec4 voronoi( in vec2 x, out vec2 resUV, out float resOcc )
{
    vec2 n = floor( x );
    vec2 f = fract( x );

	vec2 uv = vec2(0.0);
	vec4 m = vec4( 8.0 );
	float m2 = 9.0;
    for( int j=-2; j<=2; j++ )
    for( int i=-2; i<=2; i++ )
    {
        vec2 g = vec2( float(i),float(j) );
        vec2 o = hash2( n + g );
		vec2 r = g - f + o;

        // distance and tex coordinates
        vec2 u = vec2( dot( r, vec2(0.5, 0.866) ),
					   dot( r, vec2(0.5,-0.866) ) );
		vec2 d = vec2( -r.y, 1.0 );
		float h = 0.5*abs(r.x)+0.866*r.y;
		if( h > 0.0 )
		{
			u = vec2( h, r.x );
			d = vec2( 0.866*abs(r.x)+0.5*r.y, 0.5*step(0.0,r.x) );
		}

        if( d.x<m.x )
        {
			m2 = m.x;
            m.x = d.x;
            m.y = dot(n+g,vec2(7.0,113.0) );
			m.z = d.y;
			m.w = max(r.y,0.0);
			uv = u;
        }
        else if( d.x<m2 )
		{
			m2 = d.x;
        }

    }
	resUV = uv;
	resOcc = m2-m.x;
    return m;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec3 tot = vec3(0.0);

    for( int j=0; j<antialias; j++ )
    for( int i=0; i<antialias; i++ )
    {
        vec2 off = vec2(float(i),float(j))/float(antialias);

        vec2 q = (fragCoord+off)/iResolution.xy;
        vec2 p = -1.0 + 2.0*q;
        p.x *= iResolution.x/iResolution.y;
        vec2 uv = vec2(0.0);

        //vec2 dp = 2.0*vec2( 1.0/iResolution.y, 0.0 );
        vec2 dp = vec2( 0.004, 0.0 );

        float occ = 0.0;
        vec4  c = voronoi( 3.5*p, uv, occ );


        vec2 tmp; float tmp2;
        float d = abs(voronoi( 3.5*(p+dp.xy), tmp, tmp2 ).z - voronoi( 3.5*(p-dp.xy), tmp, tmp2 ).z ) +
                  abs(voronoi( 3.5*(p+dp.yx), tmp, tmp2 ).z - voronoi( 3.5*(p-dp.yx), tmp, tmp2 ).z );

        // color
        c.y = hash1( c.y );
        vec3 col = 0.6 + 0.4*sin( c.y*2.5 + 1.0+vec3(0.0,0.5,1.0) );
        col *= 0.4 + 0.6*smoothstep( 0.1,0.25,abs(hash1(c.y+0.413)-0.5) );

        // texture
        col *= 1.7*pow(texturef( uv ), vec3(0.4) );

        // lighting
        col *= clamp( 0.65 + c.z*0.35, 0.0, 1.0 );
        col *= sqrt(clamp(1.0 - c.x,0.0,1.0));
        col *= clamp(1.0-0.3*c.w, 0.0, 1.0 );
        col *= 0.8 + 0.2*vec3( sqrt(clamp(8.0*occ,0.0,1.0)) );

        tot += col;
    }

    tot /= float(antialias*antialias);

	fragColor = vec4( tot, 1.0 );
}

void main() {
    mainImage(gl_FragColor, vFragCoord);
}

    |]
