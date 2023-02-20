let employeeName = "John";
// or
let employeeName:string = "John";

var num1:number = 1;

const playerCodes = {
    player1 : 9,
    player2 : 10,
    player3 : 13,
    player4 : 20
};
playerCodes.player2 = 11; // OK

playerCodes = {     //Compiler Error: Cannot assign to playerCodes because it is a constant or read-only
    player1 : 50,   // Modified value
    player2 : 10,
    player3 : 13,
    player4 : 20
};

playerCodesArray = {     //Compiler Error: Cannot assign to playerCodes because it is a constant or read-only
    player1 : 50,   // Modified value
    player2 : playerCodes[Test],
    player3 : 13,
    player4 : 20
};


export const ROUTES: any[] = [
    {path: '/dashboard', title: 'Dashboard', icon: 'dashboard', class: '', allowAnonymous: false},
    {path: '/deals', title: 'Deals', icon: 'assignment', class: '', allowAnonymous: false},
    {path: '/pipeline', title: 'Pipeline', icon: 'timeline', class: '', allowAnonymous: false},
    {path: '/language-resolver', title: 'Language Resolver', icon: 'translate', class: '', allowAnonymous: false},
    {path: '/commit-analysis', title: 'Commit History', icon: 'tune', class: '', allowAnonymous: false},
    {path: '/login', title: 'Log In', icon: 'lock', class: '', allowAnonymous: true},
];

export const Components = _.chain([_.values(ROUTES) as any[]])
                            .flatten()
                            .filter((item) => item.name && (item.name.toLowerCase().endsWith('component')))
                            .value();

var fileLanguages = _.uniqBy([...this.fileLanguages, ...Components], p => p.fileId);

var languageMap = new Map(fileLanguages.map(lang => [lang.id, lang] as [string, ILanguage]));

let schema = mapEnumToSchema(Joi.boolean())

const codesByType = Joi.object()
  .keys({
    type: Joi.string().required(),
    limit: Joi.number().optional(),
    skip: Joi.number().optional(),
  })
  .required();
