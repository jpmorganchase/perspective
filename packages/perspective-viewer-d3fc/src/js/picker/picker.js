import {dataJoin} from "d3fc";

export default () => {
    let className = "picker";
    let svgs = {};
    let current;
    let onChange = () => {};

    const picker = selection => {
        const pickerDataJoin = dataJoin("div", className).key(d => d);

        const next = d => {
            const keys = Object.keys(svgs);
            let index = keys.indexOf(d) + 1;
            if (index >= keys.length) index = 0;
            return keys[index];
        };

        pickerDataJoin(selection, current ? [current] : [])
            .html(d => svgs[d])
            .on("click", d => onChange(next(d)));
    };

    picker.className = (...args) => {
        if (!args.length) {
            return className;
        }
        className = args[0];
        return picker;
    };
    picker.svgs = (...args) => {
        if (!args.length) {
            return svgs;
        }
        svgs = args[0];
        return picker;
    };
    picker.current = (...args) => {
        if (!args.length) {
            return current;
        }
        current = args[0];
        return picker;
    };
    picker.onChange = (...args) => {
        if (!args.length) {
            return onChange;
        }
        onChange = args[0];
        return picker;
    };

    return picker;
};
