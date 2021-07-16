/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require("react");

const CompLibrary = require("../../core/CompLibrary.js");
const MarkdownBlock = CompLibrary.MarkdownBlock; /* Used to read markdown */
const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;
const classNames = require("classnames");

const siteConfig = require(process.cwd() + "/siteConfig.js");

function imgUrl(img) {
    return siteConfig.baseUrl + "img/" + img;
}

function docUrl(doc, language) {
    return siteConfig.baseUrl + "docs/" + (language ? language + "/" : "") + doc;
}

function pageUrl(page, language) {
    return siteConfig.baseUrl + (language ? language + "/" : "") + page;
}

class Button extends React.Component {
    render() {
        return (
            <div id={this.props.id} className="pluginWrapper buttonWrapper">
                <a className="button">{this.props.children}</a>
            </div>
        );
    }
}

Button.defaultProps = {
    target: "_self"
};

const SplashContainer = props => (
    <div className="homeContainer">
        <div className="homeSplashFade">
            <div className="wrapper homeWrapper">{props.children}</div>
        </div>
    </div>
);

const Logo = props => (
    <div className="projectLogo">
        <img src={props.img_src} />
    </div>
);

const ProjectTitle = props => (
    <h2 className="projectTitle">
        <perspective-logo />
        <small>
            Streaming Analytics <i>via</i> WebAssembly
        </small>
    </h2>
);

const PromoSection = props => (
    <div className="section promoSection">
        <div className="promoRow">
            <div className="pluginRowBlock">{props.children}</div>
        </div>
    </div>
);

class HomeSplash extends React.Component {
    render() {
        let language = this.props.language || "";
        return (
            <SplashContainer>
                <div className="inner">
                    <ProjectTitle />
                    <perspective-viewer class="titleViewer nosuperstore" />

                    <PromoSection>
                        <Button id="grid">Sparkgrid</Button>
                        <Button id="grid2">Datagrid</Button>
                        <Button id="cyclone">X Bar</Button>
                        <Button id="enhance">Y Line</Button>
                        <Button id="crosssect">XY Scatter</Button>
                        <Button id="intersect">Treemap</Button>
                        <Button id="pivot">Heatmap</Button>
                    </PromoSection>
                    <input id="velocity" type="range" class="slider"></input>
                </div>
            </SplashContainer>
        );
    }
}

const Block = props => (
    <Container id={props.id} background={props.background}>
        <GridBlock contents={props.children} layout={props.layout} />
    </Container>
);

const PerspectiveBlock = props => {
    const block = props.children[0];
    let beforeImage, afterImage;
    if (block.imageAlign === "right") {
        afterImage = (
            <div className="blockImage">
                <perspective-viewer class="nosuperstore" />
            </div>
        );
    } else {
        beforeImage = (
            <div className="blockImage">
                <perspective-viewer class="nosuperstore" />
            </div>
        );
    }
    return (
        <Container padding={["top"]} id={props.id} background={props.background}>
            <div
                className={classNames({
                    imageAlignRight: !!afterImage,
                    imageAlignLeft: !!beforeImage,
                    imageAlignSide: true
                })}
                key={block.title}
            >
                {beforeImage}
                <div className="blockContent">
                    <h2>
                        <MarkdownBlock>{block.title}</MarkdownBlock>
                    </h2>
                    <MarkdownBlock layout="twoColumn">{block.content}</MarkdownBlock>
                </div>
                {afterImage}
            </div>
        </Container>
    );
};

const GalleryBlock = props => {
    const block = props.children[0];
    let beforeImage, afterImage;

    if ((siteConfig.users || []).length === 0) {
        return null;
    }

    const showcase = siteConfig.users
        .filter(user => {
            return user.pinned;
        })
        .map((user, i) => {
            return (
                <a href={user.infoLink} key={i}>
                    <img style={{width: "33.3%"}} src={user.image} alt={user.caption} title={user.caption} />
                </a>
            );
        });

    if (block.imageAlign === "right") {
        afterImage = (
            <div className="blockImage">
                <div className="logos">{showcase}</div>
            </div>
        );
    } else {
        beforeImage = (
            <div className="blockImage">
                <div className="logos">{showcase}</div>
            </div>
        );
    }
    return (
        <Container padding={["top"]} id={props.id} background={props.background}>
            <div
                className={classNames({
                    imageAlignRight: !!afterImage,
                    imageAlignLeft: !!beforeImage,
                    imageAlignSide: true
                })}
                key={block.title}
            >
                {beforeImage}
                <div className="blockContent">
                    <h2>
                        <MarkdownBlock>{block.title}</MarkdownBlock>
                    </h2>
                    <MarkdownBlock layout="twoColumn">{block.content}</MarkdownBlock>
                </div>
                {afterImage}
            </div>
        </Container>
    );
};

const YoutubeBlock = props => {
    const block = props.children[0];
    let beforeImage, afterImage;
    const url = "https://www.youtube.com/embed/IO-HJsGdleE?&theme=dark&autohide=1&modestbranding=1&showinfo=0&rel=0";
    if (block.imageAlign === "right") {
        afterImage = (
            <div className="blockImage">
                <div className="youtube">
                    <iframe width="500" height="294" src={url} frameBorder="0"></iframe>
                </div>
            </div>
        );
    } else {
        beforeImage = (
            <div className="blockImage">
                <div className="youtube">
                    <iframe width="500" height="294" src={url} frameBorder="0"></iframe>
                </div>
            </div>
        );
    }
    return (
        <Container padding={["bottom"]} id={props.id} background={props.background}>
            <div
                className={classNames({
                    imageAlignRight: !!afterImage,
                    imageAlignLeft: !!beforeImage,
                    imageAlignSide: true
                })}
                key={block.title}
            >
                {beforeImage}
                <div className="blockContent">
                    <h2>
                        <MarkdownBlock>{block.title}</MarkdownBlock>
                    </h2>
                    <MarkdownBlock>{block.content}</MarkdownBlock>
                </div>
                {afterImage}
            </div>
        </Container>
    );
};

const Features = props => (
    <Block layout="fourColumn">
        {[
            {
                image: "https://bl.ocks.org/texodus/raw/803de90736a3641ad91c5c7a1b49d0a7/thumbnail.png",
                title: "Simple"
            },
            {
                content: "Utilizing bleeding-edge browser technology such as Web Assembly and Apache Arrow, Perspective is unmatched in browser performance",
                image: imgUrl("baseline-trending_up-24px.svg"),
                imageAlign: "top",
                title: "Powerful"
            },
            {
                content: "Engineered for reliability and production-vetted on the J.P. Morgan trading floor, now available to the development community as Open Source",
                image: imgUrl("baseline-security-24px.svg"),
                imageAlign: "top",
                title: "Industrial"
            }
        ]}
    </Block>
);

// const FeatureCallout = props => (
//     <Container padding={["bottom", "top"]} background="dark">
//         <h2>Features</h2>
//         <MarkdownBlock background="dark">
//             A fast, memory efficient streaming pivot engine written principally in C++ and compiled to WebAssembly via the [emscripten](https://github.com/kripken/emscripten) compiler.
//         </MarkdownBlock>
//         <MarkdownBlock>
//             An embeddable, framework-agnostic configuration UI, based on [Web Components](https://www.webcomponents.org/), and a WebWorker engine host for responsiveness at high frequency.
//         </MarkdownBlock>
//         <MarkdownBlock>A customizable HTML Data Grid plugin, and a Charts plugin built on [D3FC](https://d3fc.io/).</MarkdownBlock>
//         <MarkdownBlock>Integration with Jupyterlab, both natively in a Python kernel, and as a notebook Widget.</MarkdownBlock>
//         <MarkdownBlock>Cross-language streaming & virtualization to the browser via [Apache Arrow](https://arrow.apache.org/).</MarkdownBlock>
//         <MarkdownBlock>Runtimes for the browser, Python, and Node.js.</MarkdownBlock>
//     </Container>
// );

const DESCRIPTION_TEXT = `
# What is Perspective?
Perspective is an <i>interactive</i> visualization component for <i>large</i>, <i>real-time</i>
datasets. Originally developed at J.P. Morgan,  Perspective
makes it simple to build real-time & user configurable analytics entirely in the
browser, or in concert with Python and/or
[Jupyterlab](https://jupyterlab.readthedocs.io/en/stable/).
Use it to create reports, dashboards, notebooks and applications, with static
data or streaming updates via [Apache Arrow](https://arrow.apache.org/).  As a
library, Perspective provides both:

* A fast, memory efficient streaming query engine, written in
  C++ and compiled for both [WebAssembly](https://webassembly.org/) and
  [Python](https://www.python.org/), with read/write/stream/virtual support for
  [Apache Arrow](https://arrow.apache.org/).

* A framework-agnostic User Interface
  [Custom Element](https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_custom_elements)
  and [Jupyterlab](https://jupyterlab.readthedocs.io/en/stable/) Widget, via
  WebWorker (WebAssembly) or virtually via WebSocket (Python/Node), and a suite of
  Datagrid and [D3FC](https://d3fc.io/) Chart plugins.
`;

const Description = props => (
    <GalleryBlock id="demo1">
        {[
            {
                content: DESCRIPTION_TEXT,
                imageAlign: "right"
            }
        ]}
    </GalleryBlock>
);

const PYTHON_TEXT = `
## Python
\`perspective-python\`, built on the same C++ data engine used by the
[WebAssembly version](https://perspective.finos.org/docs/md/js.html), implements
the Perspective API directly in Python, either as a high-performance 
Streaming Arrow or Virtualized Server for Production, or as an embedded
JupyterLab Widget for Research.

For Application Developers, virtualized \`<perspective-viewer>\` will only
consume the  data necessary to render the current screen, enabling _ludicrous_ _size_
datasets with instant-load after they've been server initialzed.  Or - Clone
the entire dataset to the WebAssembly runtime via efficiently via Arrow, and
give your server a break!

For Researchers and Data Scientists,  \`PerspectiveWidget\` is available as an
embedded widget within [JupyterLab](https://jupyterlab.readthedocs.io/en/stable/),
allowing [Pandas](https://pandas.pydata.org/) and Arrow preview, transform,
export and persist Perspective visualizations _interactively_.
`;

const Jupyter = props => (
    <YoutubeBlock id="demo1">
        {[
            {
                content: PYTHON_TEXT,
                imageAlign: "right"
            }
        ]}
    </YoutubeBlock>
);

const JS_TEXT = `
## Web Browser
Query-driven dashboards built on Perspective
[Custom Elements](https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_custom_elements)
of JavaScript are completely user-configurable, and easy to integrate into any
web application framework.

Using Perspective's simple _relational_ grammar, elements like 
\`<perspective-workspace>\` can be _symmetrically_ configured, by API or
through the User Interface, and emit dataset-aware Events for scriptable
interactivity.  Web Applications built in JavaScript with Perspective
Custom Elements can be re-hydrated from their serialized state, driven
from external Events, or persisted to any store.  Workspaces can mix
virtual, server-side Python data with in-browser client data seamlessly, and
independent data Views can be cross-filtered, duplicated, exported, stacked
and saved.

To achieve Desktop-like performance in the Browser, Perspective for JavaScript
relies on [WebAssembly](https://webassembly.org/) for excellent
_query calculation_ time, and [Apache Arrow](https://arrow.apache.org/)
for its conservative _memory footprint_ and efficient _data serialization_.
`;
const Javascript = props => (
    <Block id="javascript">
        {[
            {
                content: JS_TEXT,
                image: imgUrl("demo_small.gif"),
                imageAlign: "left"
            }
        ]}
    </Block>
);

// const GETTING_STARTED_TEXT = `
// # Get Started
// 1. Add \`@finos/perspective-cli\` to your project:
// \`\`\`bash
// $ yarn add --dev @finos/perspective-cli
// \`\`\`
// 2. Run a test server on a CSV, JSON or [Apache Arrow](https://arrow.apache.org/):
// \`\`\`bash
// $ yarn perspective host < superstore.arrow
// Listening on port 8080
// \`\`\`
// `;

const GetStarted = props => (
    <Container padding={["bottom", "top"]} id={props.id} background="dark">
        <div id="get_started">
            <perspective-viewer class="nosuperstore"></perspective-viewer>
        </div>
    </Container>
);

const Testimonials = props => {
    return (
        <div className="testimonials">
            <Container padding={["bottom", "top"]}>
                <GridBlock
                    align="center"
                    contents={[
                        {
                            image: `https://bl.ocks.org/texodus/raw/803de90736a3641ad91c5c7a1b49d0a7/preview.png`,
                            infoLink: "https://bl.ocks.org/texodus/803de90736a3641ad91c5c7a1b49d0a7",
                            imageAlign: "top",
                            title: 'Superstore <br/><font size="2">Static Apache Arrow Example</font>'
                        },
                        {
                            content:
                                "*Open source contributions to the React Native docs have skyrocketed after our move to Docusaurus. The docs are now hosted on a small repo in plain markdown, with none of the clutter that a typical static site generator would require. Thanks Slash!*",
                            image: `${siteConfig.baseUrl}img/hector-ramos.png`,
                            imageAlign: "top",
                            imageAlt: "Hector Ramos",
                            title: 'Hector Ramos <br/><font size="2">Lead React Native Advocate</font>'
                        },
                        {
                            content:
                                "*Docusaurus has been a great choice for the ReasonML family of projects. It makes our documentation consistent, i18n-friendly, easy to maintain, and friendly for new contributors.*",
                            image: `${siteConfig.baseUrl}img/ricky-vetter.jpg`,
                            imageAlign: "top",
                            imageAlt: "Ricky Vetter",
                            title: 'Ricky Vetter <br/><font size="2">ReasonReact Developer</font>'
                        }
                    ]}
                    layout="threeColumn"
                />
            </Container>
        </div>
    );
};

const Showcase = props => {
    if ((siteConfig.users || []).length === 0) {
        return null;
    }

    const showcase = siteConfig.users
        .filter(user => {
            return user.pinned;
        })
        .map((user, i) => {
            return (
                <a href={user.infoLink} key={i}>
                    <h4>{user.caption}</h4>
                    <img src={user.image} alt={user.caption} title={user.caption} />
                </a>
            );
        });

    return (
        <div className="productShowcaseSection paddingBottom">
            <h2>{"Examples"}</h2>
            <div className="logos">{showcase}</div>
        </div>
    );
};

const ChartTypes = props => {
    if ((siteConfig.users || []).length === 0) {
        return null;
    }

    const showcase_light = Array.from(Array(75).keys()).map((user, i) => {
        return (
            <a className="feature" key={i} data-key={i}>
                <img src={`features/feature_${i}.png`} />
            </a>
        );
    });

    const showcase_dark = Array.from(Array(75).keys()).map((user, i) => {
        return (
            <a className="feature" key={i} data-key={i}>
                <img src={`features/feature_${i}_dark.png`} />
            </a>
        );
    });

    return (
        <div className="productShowcaseSection paddingBottom">
            <Button id="switch_theme">Switch to Dark Theme</Button>
            <div id="light" className="logos">
                {showcase_light}
            </div>

            <div id="dark" className="logos">
                {showcase_dark}
            </div>
        </div>
    );
};

class Index extends React.Component {
    render() {
        let language = this.props.language || "";

        return (
            <div>
                <HomeSplash language={language} />
                <Description />
                <Javascript />
                <Jupyter />
                {/* <Showcase /> */}
                <ChartTypes />
                {/* <GetStarted /> */}
            </div>
        );
    }
}

module.exports = Index;
