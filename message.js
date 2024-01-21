"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.parseLintResult = void 0;
const parseLintResult = (result, filepath) => {
    const list = [];
    const fileResult = result.filter((r) => r.filePath === filepath);
    fileResult.forEach((r) => {
        r.messages.forEach((m) => {
            if (!m.ruleId || !m.severity)
                return;
            list.push({
                ruleId: m.ruleId,
                severity: m.severity,
                message: m.message,
                line: m.line,
                column: m.column,
                endLine: m.endLine,
                endColumn: m.endColumn,
            });
        });
    });
    return list;
};
exports.parseLintResult = parseLintResult;
