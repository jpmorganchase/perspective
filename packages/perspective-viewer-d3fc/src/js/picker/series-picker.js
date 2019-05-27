import {rebindAll} from "d3fc";
import picker from "./picker";

import lineSvg from "./line-chart-svg";
import columnSvg from "./column-chart-svg";
import areaSvg from "./area-chart-svg";
import yscatterSvg from "./yscatter-chart-svg";

const typeSvgs = {
    line: lineSvg,
    column: columnSvg,
    area: areaSvg,
    yscatter: yscatterSvg
};

export default () => {
    const base = picker()
        .className("series-picker")
        .svgs(typeSvgs);

    const seriesPicker = selection => {
        base(selection);
    };

    rebindAll(seriesPicker, base);
    return seriesPicker;
};
